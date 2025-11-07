type Maybe<T> = { tag: "Just"; value: T } | { tag: "Nothing" };
export const Just = <T>(value: T): Maybe<T> => ({ tag: "Just", value });
export const Nothing: Maybe<never> = { tag: "Nothing" };
export const fold = <T>(
  m: Maybe<T>,
  onNothing: () => void,
  onJust: (value: T) => void
) => (m.tag === "Just" ? onJust(m.value) : onNothing());
