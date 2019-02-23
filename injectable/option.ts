interface None {
  kind: "none"
}
interface Some<A> {
  kind: "some"
  value: A
}
export type Option<A> = None | Some<A>
export const isNone = <A>(a: Option<A>): a is None => a.kind === "none"
export const isSome = <A>(a: Option<A>): a is Some<A> => a.kind === "some"
export const none: None = { kind: "none" }
export const some = <A>(a: A): Some<A> => ({ kind: "some", value: a })
export const getOrElse = <A, B>(b: B) => (a: Option<A>): A | B =>
  isSome(a) ? a.value : b
export const chain = <A, B>(f: (arg0: A) => Option<B>) => (
  a: Option<A>,
): Option<B> => (isSome(a) ? f(a.value) : none)
export const apply = <A, B>(f: Option<(arg0: A) => B>) => (
  a: Option<A>,
): Option<B> => (isSome(f) && isSome(a) ? some(f.value(a.value)) : none)
export const map = <A, B>(f: (arg0: A) => B) => (a: Option<A>): Option<B> =>
  isSome(a) ? some(f(a.value)) : none
export const of = <A>(a: A): Option<A> => some(a)
// export const compose = <R>(fn1: (a: R) => R, ...fns: Array<(a: R) => R>) =>
//   fns.reduce((prevFn, nextFn) => value => prevFn(nextFn(value)), fn1)
// export const compose = <A, B, C>(fn1: (a: A) => B, fn2: (b: B) => C) => (
//   a: A,
// ): C => fn2(fn1(a))
