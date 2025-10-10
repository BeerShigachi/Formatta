import * as vscode from "vscode";
import { execFile } from "child_process";
import * as path from "path";

type Option<T> = { tag: "Some"; value: T } | { tag: "None" };
const Some = <T>(value: T): Option<T> => ({ tag: "Some", value });
const None: Option<never> = { tag: "None" };
const fold = <T>(
  opt: Option<T>,
  onNone: () => void,
  onSome: (value: T) => void
) => (opt.tag === "Some" ? onSome(opt.value) : onNone());

export function activate(context: vscode.ExtensionContext) {
  const excutablePath = path.join(context.extensionPath, "bin", "Formatta");
  const registerCommand =
    (context: vscode.ExtensionContext) =>
    (command: string) =>
    (handler: () => void) => {
      const disposable = vscode.commands.registerCommand(command, handler);
      context.subscriptions.push(disposable);
    };

  const handleCliResult = (output: string) => {
    const result = { true: true, false: false }[output.trim()];
    return result
      ? () => {
          vscode.workspace
            .getConfiguration("editor")
            .update("formatOnSave", result, vscode.ConfigurationTarget.Global);
          vscode.window.showInformationMessage(
            `Format On Save set to: ${result}`
          );
        }
      : () => {
          vscode.window.showErrorMessage(
            "Formatta CLI returned invalid output: " + output.trim()
          );
        };
  };

  const handleExecResult = (error: Error | null, stdout: string) =>
    error
      ? Some(() =>
          vscode.window.showErrorMessage("Formatta CLI error: " + error.message)
        )
      : Some(handleCliResult(stdout));

  const formatOnSaveHandler = () => {
    const current = vscode.workspace
      .getConfiguration("editor")
      .get("formatOnSave");
    execFile(
      excutablePath,
      ["toggle", String(current).toLowerCase()],
      (error, stdout, stderr) => {
        fold(
          handleExecResult(error, stdout),
          () => {},
          (fn) => fn()
        );
      }
    );
  };

  registerCommand(context)("formatta.toggleFormatOnSave")(formatOnSaveHandler);
}

export function deactivate() {}
