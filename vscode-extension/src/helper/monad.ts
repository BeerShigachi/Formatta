export type Maybe<T> = { tag: "Just"; value: T } | { tag: "Nothing" };
export const Just = <T>(value: T): Maybe<T> => ({ tag: "Just", value });
export const Nothing: Maybe<never> = { tag: "Nothing" };
export const maybe = <T, R>(
  m: Maybe<T>,
  onNothing: () => R,
  onJust: (value: T) => R
): R => (m.tag === "Just" ? onJust(m.value) : onNothing());
export const fmap = <T, U>(m: Maybe<T>, fn: (value: T) => U): Maybe<U> =>
  m.tag === "Just" ? Just(fn(m.value)) : Nothing;
export type Either<T, E> = { tag: "Ok"; value: T } | { tag: "Err"; error: E };
export const Ok = <T>(value: T): Either<T, never> => ({ tag: "Ok", value });
export const Err = <E>(error: E): Either<never, E> => ({ tag: "Err", error });
export const either = <E, T, R>(
  onLeft: (error: E) => R,
  onRight: (value: T) => R,
  e: Either<T, E>
): R => (e.tag === "Ok" ? onRight(e.value) : onLeft(e.error));
