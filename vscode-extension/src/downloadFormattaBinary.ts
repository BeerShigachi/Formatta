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

const fetchToFile = (url: string, filePath: string) =>
  fetch(url).then((res) =>
    !res.ok
      ? Promise.reject(new Error(`Failed to fetch: ${res.statusText}`))
      : new Promise<void>((resolve, reject) => {
          if (!res.body) return reject(new Error("No response body"));
          const fileStream = fs.createWriteStream(filePath, { mode: 0o755 });
          res.body.pipe(fileStream);
          res.body.on("error", reject);
          fileStream.on("finish", () => resolve());
        })
  );

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

export async function downloadFormattaBinary(
  context: vscode.ExtensionContext
): Promise<string> {
  const platform = os.platform();
  const binNameMaybe = platformBinName(platform);
  const binPathMaybe = fmap(binNameMaybe, (name) =>
    context.asAbsolutePath(path.join("bin", name))
  );
  const binName = maybe(
    binNameMaybe,
    () => {
      throw new Error(`Unsupported platform: ${platform}`);
    },
    (name) => name
  );
  const binPath = maybe(
    binPathMaybe,
    () => {
      throw new Error(`Unsupported platform: ${platform}`);
    },
    (p) => p
  );
  const binUrl = `${releaseBase}/${binName}`;
  const hashUrl = `${binUrl}.sha256`;

  const ensureDownloaded = () =>
    maybe(
      maybeFile(binPath),
      () =>
        vscode.window
          .showInformationMessage(`Downloading ${path.basename(binPath)}`)
          .then(() => fetchToFile(binUrl, binPath)),
      () => Promise.resolve()
    );

  await ensureDownloaded();
  const [expectedHashResult, actualHash] = await Promise.all([
    fetchText(hashUrl),
    Promise.resolve(sha256(binPath))
  ]);
  const expectedHash = either(
    (err) => {
      throw new Error(`Failed to fetch hash: ${err}`);
    },
    (h) => h.trim(),
    expectedHashResult
  );
  if (actualHash !== expectedHash) {
    fs.unlinkSync(binPath);
    throw new Error("Downloaded binary failed hash check and was deleted.");
  }
  return binPath;
}
