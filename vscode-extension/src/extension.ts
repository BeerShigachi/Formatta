import * as vscode from "vscode";
import { execFile } from "child_process";
import * as path from "path";
import { Some, fold } from "./helper/monad";

type StatusConfig = { icon: string; color: string };

const STATUS_CONFIG: Record<string, StatusConfig> = {
  play: { icon: "$(play) Formatta", color: "" },
  pause: { icon: "$(debug-pause) Formatta", color: "orange" },
};

const updateStatusBarIcon = (
  item: vscode.StatusBarItem,
  isFormatOnSave: boolean,
) => {
  const configOpt = isFormatOnSave
    ? STATUS_CONFIG["play"]
    : STATUS_CONFIG["pause"];
  item.text = configOpt.icon;
  item.color = configOpt.color;
  item.tooltip = isFormatOnSave ? "Formatting: Enabled" : "Formatting: Paused";
};

export function activate(context: vscode.ExtensionContext) {
  const statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100,
  );
  const initial = vscode.workspace
    .getConfiguration("editor")
    .get("formatOnSave");
  updateStatusBarIcon(statusBarItem, Boolean(initial));
  statusBarItem.command = "formatta.toggleFormatOnSave";
  statusBarItem.show();
  context.subscriptions.push(statusBarItem);

  const EXCUTABLE_PATH = path.join(context.extensionPath, "bin", "Formatta");
  const registerCommand =
    (context: vscode.ExtensionContext) =>
    (command: string) =>
    (handler: () => void) => {
      const disposable = vscode.commands.registerCommand(command, handler);
      context.subscriptions.push(disposable);
    };

  const handleCliResult = (output: string) => {
    const result = { true: true, false: false }[output.trim()];
    return result !== undefined
      ? () => {
          vscode.workspace
            .getConfiguration("editor")
            .update("formatOnSave", result, vscode.ConfigurationTarget.Global);
          updateStatusBarIcon(statusBarItem, result);
          vscode.window.showInformationMessage(
            `Format On Save: ${result ? "Enabled" : "Paused"}`,
          );
        }
      : () => {
          vscode.window.showErrorMessage(
            "Formatta CLI returned invalid output: " + output.trim(),
          );
        };
  };

  const handleExecResult = (error: Error | null, stdout: string) =>
    error
      ? Some(() =>
          vscode.window.showErrorMessage(
            "Formatta CLI error: " + error.message,
          ),
        )
      : Some(handleCliResult(stdout));

  const formatOnSaveHandler = () => {
    const current = vscode.workspace
      .getConfiguration("editor")
      .get("formatOnSave");
    execFile(
      EXCUTABLE_PATH,
      ["toggle", String(current).toLowerCase()],
      (error, stdout, stderr) => {
        fold(
          handleExecResult(error, stdout),
          () => {},
          (fn) => fn(),
        );
      },
    );
  };

  registerCommand(context)("formatta.toggleFormatOnSave")(formatOnSaveHandler);
}

export function deactivate() {}
