import * as os from "os";
import * as path from "path";
import * as fs from "fs";
import fetch from "node-fetch";
import { createHash } from "crypto";
import * as vscode from "vscode";
import {
  Maybe,
  Just,
  Nothing,
  maybe,
  fmap,
  Either,
  Ok,
  Err,
  either
} from "./helper/monad";

const releaseBase =
  "https://github.com/BeerShigachi/Formatta/releases/latest/download";

const BIN_MAP: Record<string, string> = {
  darwin: "formatta-macos",
  win32: "formatta-windows.exe",
  linux: "formatta-linux"
};

const platformBinName = (platform: string): Maybe<string> => {
  const bin = BIN_MAP[platform];
  return bin ? Just(bin) : Nothing;
};

const fetchToFile = (
  url: string,
  filePath: string
): Promise<Either<void, string>> =>
  fetch(url)
    .then((res) =>
      res.ok && res.body
        ? new Promise<Either<void, string>>((resolve) => {
            const fileStream = fs.createWriteStream(filePath, { mode: 0o755 });
            res.body?.pipe(fileStream);
            res.body?.on("error", () => resolve(Err("Stream error")));
            fileStream.on("finish", () => resolve(Ok(undefined)));
          })
        : Promise.resolve(
            Err(
              !res.ok
                ? `Failed to fetch: ${res.statusText}`
                : "No response body"
            )
          )
    )
    .catch((e) => Err(e?.message || "Unknown error in fetchToFile"));

const maybeFile = (filePath: string): Maybe<string> =>
  fs.existsSync(filePath) ? Just(filePath) : Nothing;

const fetchText = (url: string): Promise<Either<string, string>> =>
  fetch(url)
    .then((res) =>
      res.ok
        ? res.text().then((text) => Ok<string, string>(text))
        : Promise.resolve(
            Err<string, string>(`Failed to fetch: ${res.statusText}`)
          )
    )
    .catch((e) =>
      Err<string, string>(e?.message || "Unknown error in fetchText")
    );

const sha256 = (filePath: string) =>
  createHash("sha256").update(fs.readFileSync(filePath)).digest("hex");

export function getFormattaBinaryPath(
  context: vscode.ExtensionContext
): string {
  const platform = os.platform();
  const binNameMaybe = platformBinName(platform);
  const binPathMaybe = fmap(binNameMaybe, (name) =>
    context.asAbsolutePath(name)
  );
  return maybe<string, string>(() => {
    throw new Error(`Unsupported platform: ${platform}`);
  })((p) => p)(binPathMaybe);
}

export async function downloadFormattaBinary(
  context: vscode.ExtensionContext
): Promise<string> {
  const binPath = getFormattaBinaryPath(context);
  const binName = path.basename(binPath);
  const binUrl = `${releaseBase}/${binName}`;
  const hashUrl = `${binUrl}.sha256`;

  const ensureDownloaded = () =>
    maybe<string, Promise<Either<void, string>>>(() =>
      Promise.resolve(
        vscode.window
          .showInformationMessage(`Downloading ${path.basename(binPath)}`)
          .then(() => fetchToFile(binUrl, binPath))
      )
    )(() => Promise.resolve(Ok(undefined)))(maybeFile(binPath));

  // refactor later to use monad operations
  const downloadResult = await ensureDownloaded();
  if (downloadResult.tag === "Ok") {
    vscode.window.showInformationMessage(
      `Formatta binary downloaded: ${binPath}`
    );
  } else {
    vscode.window.showErrorMessage(
      `Failed to download Formatta binary: ${downloadResult.error ?? "Unknown error"}`
    );
  }
  const [expectedHashResult, actualHash] = await Promise.all([
    fetchText(hashUrl),
    Promise.resolve(sha256(binPath))
  ]);

  const throwError = (prefix: string) => (err: string) => {
    throw new Error(`${prefix}${err}`);
  };
  const checkHash = (expected: string, actual: string, path: string) =>
    actual === expected
      ? Ok<string, string>(path)
      : (fs.unlinkSync(path),
        Err<string, string>(
          "Downloaded binary failed hash check and was deleted."
        ));

  return either<string, string, string>(throwError(""))((p) => p)(
    checkHash(
      either<string, string, string>(throwError("Failed to fetch hash: "))(
        (h) => h.trim()
      )(expectedHashResult),
      actualHash,
      binPath
    )
  );
}
