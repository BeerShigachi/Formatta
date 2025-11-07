// types
export type Either<T, E> = { tag: "Ok"; value: T } | { tag: "Err"; error: E };
export type Maybe<T> = { tag: "Just"; value: T } | { tag: "Nothing" };

// data constructors
export const Just = <T>(value: T): Maybe<T> => ({ tag: "Just", value });
export const Nothing: Maybe<never> = { tag: "Nothing" };

export const Ok = <T, E = never>(value: T): Either<T, E> => ({
  tag: "Ok",
  value
});
export const Err = <E, T = never>(error: E): Either<T, E> => ({
  tag: "Err",
  error
});

// Algebraic data types (ADTs) operations
export const maybe = <T, R>(
  m: Maybe<T>,
  onNothing: () => R,
  onJust: (value: T) => R
): R => (m.tag === "Just" ? onJust(m.value) : onNothing());
export const fmap = <T, U>(m: Maybe<T>, fn: (value: T) => U): Maybe<U> =>
  m.tag === "Just" ? Just(fn(m.value)) : Nothing;

export const either = <E, T, R>(
  onLeft: (error: E) => R,
  onRight: (value: T) => R,
  e: Either<T, E>
): R => (e.tag === "Ok" ? onRight(e.value) : onLeft(e.error));
