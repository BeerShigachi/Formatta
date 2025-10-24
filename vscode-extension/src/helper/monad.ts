type Option<T> = { tag: "Some"; value: T } | { tag: "None" };
export const Some = <T>(value: T): Option<T> => ({ tag: "Some", value });
export const None: Option<never> = { tag: "None" };
export const fold = <T>(
  opt: Option<T>,
  onNone: () => void,
  onSome: (value: T) => void,
) => (opt.tag === "Some" ? onSome(opt.value) : onNone());
