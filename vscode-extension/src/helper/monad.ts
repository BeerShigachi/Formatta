export type Maybe<T> = { tag: "Just"; value: T } | { tag: "Nothing" };
export const Just = <T>(value: T): Maybe<T> => ({ tag: "Just", value });
export const Nothing: Maybe<never> = { tag: "Nothing" };
export const fold = <T, R>(
  m: Maybe<T>,
  onNothing: () => R,
  onJust: (value: T) => R
): R => (m.tag === "Just" ? onJust(m.value) : onNothing());
