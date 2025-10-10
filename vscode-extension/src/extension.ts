import * as vscode from "vscode";
import { execFile } from "child_process";
import * as path from "path";
// Option monad (module-level)
type Option<T> = { tag: "Some"; value: T } | { tag: "None" };
const Some = <T>(value: T): Option<T> => ({ tag: "Some", value });

export function activate(context: vscode.ExtensionContext) {
  const excutablePath = path.join(context.extensionPath, "bin", "Formatta");

  const registerCommand =
    (context: vscode.ExtensionContext) =>
    (command: string) =>
    (handler: () => void) => {
      const disposable = vscode.commands.registerCommand(command, handler);
      context.subscriptions.push(disposable);
    };

  const formatOnSaveHandler = () => {
    // Get the current formatOnSave value
    const current = vscode.workspace
      .getConfiguration("editor")
      .get("formatOnSave");
    // Call CLI with 'toggle <true|false>'
    execFile(
      excutablePath,
      ["toggle", String(current).toLowerCase()],
      (error, stdout, stderr) => {
        if (error) {
          vscode.window.showErrorMessage(
            "Formatta CLI error: " + error.message
          );
          return;
        }
        const result = stdout.trim();
        if (result === "true" || result === "false") {
          vscode.workspace
            .getConfiguration("editor")
            .update(
              "formatOnSave",
              result === "true",
              vscode.ConfigurationTarget.Global
            );
          vscode.window.showInformationMessage(
            `Format On Save set to: ${result}`
          );
        } else {
          vscode.window.showErrorMessage(
            "Formatta CLI returned invalid output: " + result
          );
        }
      }
    );
  };

  // Register command
  registerCommand(context)("formatta.toggleFormatOnSave")(formatOnSaveHandler);
}

export function deactivate() {}
