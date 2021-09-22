#!/usr/bin/env tsc

// ts-node src/untyped.ts
// tsc --target es2017 --sourcemap src/untyped.ts

type Expr = Num | Var | Lam | App | Let | Add | Sub | Mul | Eq | Err
interface Num { kind: 'Num', k: number }
interface Var { kind: 'Var', x: string }
interface Let { kind: 'Let', vars: Env, e: Expr }
interface Lam { kind: 'Lam', x: string, e: Expr }
interface App { kind: 'App', e1: Expr, e2: Expr }
interface Add { kind: 'Add' }
interface Sub { kind: 'Sub' }
interface Mul { kind: 'Mul' }
interface Eq { kind: 'Eq' }
interface Err { kind: 'Err', err: string }

type Env = { [x: string]: Expr }

const Num = (k: number): Expr => ({ kind: 'Num', k: k })
const Var = (x: string): Expr => ({ kind: 'Var', x: x })
const Let = (vars: Env, e: Expr): Expr => ({ kind: 'Let', vars: vars, e: e })
const Lam = (x: string, e: Expr): Expr => ({ kind: 'Lam', x: x, e: e })
const App = (e1: Expr, e2: Expr): Expr => ({ kind: 'App', e1: e1, e2: e2 })
const Add = (): Expr => ({ kind: 'Add' })
const Sub = (): Expr => ({ kind: 'Sub' })
const Mul = (): Expr => ({ kind: 'Mul' })
const Eq = (): Expr => ({ kind: 'Eq' })
const Err = (err: string): Expr => ({ kind: 'Err', err: err })

const app = (f: Expr, xs: Expr[]): Expr => xs.reduce(App, f)
const add = (e1: Expr, e2: Expr): Expr => App(App(Add(), e1), e2)
const sub = (e1: Expr, e2: Expr): Expr => App(App(Sub(), e1), e2)
const mul = (e1: Expr, e2: Expr): Expr => App(App(Mul(), e1), e2)
const eq = (e1: Expr, e2: Expr): Expr => App(App(Eq(), e1), e2)

const evaluate = (expr: Expr, env: Env): Expr => {
    switch (expr.kind) {
        case 'Num': return Num(expr.k)
        case 'Var': {
            let ex = env[expr.x]
            if (ex === undefined)
                return Err(`Undefined variable: ${expr.x}`)
            if (equals(ex, Var(expr.x)))
                return Var(expr.x)
            return evaluate(ex, { ...env, [expr.x]: Var(expr.x) })
        }
        case 'Let': return evaluate(expr.e, { ...env, ...expr.vars })
        case 'Lam': {
            let e = evaluate(expr.e, { ...env, [expr.x]: Var(expr.x) })
            if (e.kind === 'Err') return Err(e.err)
            return Lam(expr.x, e)
        }
        case 'App': {
            let e1 = evaluate(expr.e1, env)
            switch (e1.kind) {
                case 'Num': return Err(`Not a function: ${show(Num(e1.k))}`)
                case 'Lam': return evaluate(e1.e, { ...env, [e1.x]: Let(env, expr.e2) })
                case 'Err': return Err(e1.err)
            }
            let e2 = evaluate(expr.e2, env)
            if (e2.kind === 'Err') return Err(e2.err)
            switch (e1.kind) {
                case 'Var': return App(Var(e1.x), e2)
                case 'App':
                    if (e1.e1.kind === 'Add' && e1.e2.kind === 'Num' && e2.kind === 'Num') return Num(e1.e2.k + e2.k)
                    if (e1.e1.kind === 'Sub' && e1.e2.kind === 'Num' && e2.kind === 'Num') return Num(e1.e2.k - e2.k)
                    if (e1.e1.kind === 'Mul' && e1.e2.kind === 'Num' && e2.kind === 'Num') return Num(e1.e2.k * e2.k)
                    if (e1.e1.kind === 'Eq' && e1.e2.kind === 'Num' && e2.kind === 'Num') {
                        if (e1.e2.k == e2.k)
                            return Lam("True", Lam("False", Var("True")))
                        return Lam("True", Lam("False", Var("False")))
                    }
            }
            return App(e1, e2)
        }
        case 'Add': case 'Sub': case 'Mul': case 'Eq': case 'Err': return expr
    }
    throw `⚠️ evaluate: not implemented: ${JSON.stringify(expr)}`
}

//-------------------------------------------
// BOILERPLATE FUNCTIONS

const equals = (a: Expr, b: Expr): boolean => {
    if (a.kind !== b.kind) return false
    if (a.kind === 'Num' && b.kind === 'Num')
        return a.k === b.k
    if (a.kind === 'Var' && b.kind === 'Var')
        return a.x === b.x
    if (a.kind === 'Lam' && b.kind === 'Lam')
        return a.x === b.x && equals(a.e, b.e)
    if (a.kind === 'App' && b.kind === 'App')
        return equals(a.e1, b.e1) && equals(a.e2, b.e2)
    if (a.kind === 'Err' && b.kind === 'Err')
        return a.err === b.err
    return true
}

const show = (expr: Expr): string => {
    switch (expr.kind) {
        case 'Num': return `${expr.k}`
        case 'Var': return expr.x
        case 'Let':
            if (Object.keys(expr.vars).length == 0)
                return show(expr.e)
            return `${showEnv(expr.vars)}; ${show(expr.e)}`
        case 'Lam': return `λ${expr.x}. ${show(expr.e)}`
        case 'App':
            if (expr.e1.kind === 'Var' && expr.e2.kind === 'Lam')
                return `${show(expr.e1)} (${show(expr.e2)})`
            if (expr.e1.kind === 'Var' && expr.e2.kind === 'App')
                return `${show(expr.e1)} (${show(expr.e2)})`
            if (expr.e1.kind === 'Lam' && expr.e2.kind === 'Num')
                return `(${show(expr.e1)}) ${show(expr.e2)}`
            if (expr.e1.kind === 'Lam' && expr.e2.kind === 'Var')
                return `(${show(expr.e1)}) ${show(expr.e2)}`
            if (expr.e1.kind === 'Lam' && expr.e2.kind === 'Lam')
                return `(${show(expr.e1)}) (${show(expr.e2)})`
            if (expr.e1.kind === 'Lam' && expr.e2.kind === 'App')
                return `(${show(expr.e1)}) (${show(expr.e2)})`
            if (expr.e1.kind === 'App' && expr.e2.kind === 'Lam')
                return `${show(expr.e1)} (${show(expr.e2)})`
            if (expr.e1.kind === 'App' && expr.e2.kind === 'App')
                return `${show(expr.e1)} (${show(expr.e2)})`
            return `${show(expr.e1)} ${show(expr.e2)}`
        case 'Add': return '+'
        case 'Sub': return '-'
        case 'Mul': return '*'
        case 'Eq': return '=='
        case 'Err': return expr.err
    }
    throw `⚠️ show: not implemented: ${JSON.stringify(expr)}`
}

const showEnv = (env: Env): string =>
    Object.entries(env)
        .map(([x, expr], _) => `${x} = ${show(expr)}`)
        .join('; ')

const check = (e: Expr, env: Env, expected: Expr) => {
    let got = evaluate(e, env)
    if (equals(got, expected))
        console.log(`✅ ${show(e)}  ⊣  Γ[${showEnv(env)}]  ∴  ${show(got)}`)
    else
        console.error(`❌ ${show(e)} {${showEnv(env)}} -- got '${show(got)}'; expected '${show(expected)}'`)
}

//-------------------------------------------
// TESTS
console.log('\n☯︎ Number')
check(Num(1), {}, Num(1))

console.log('\n☯︎ Variable')
check(Var("x"), {}, Err("Undefined variable: x"))
check(Var("x"), { x: Num(1) }, Num(1))
check(Var("x"), { x: Var("x") }, Var("x"))
check(Var("x"), { x: Var("y"), y: Num(1) }, Num(1))
check(Var("x"), { y: Num(1), x: Var("y") }, Num(1))

console.log('\n☯︎ Let bindings')
check(Let({}, Var("x")), { "x": Num(1) }, Num(1))
check(Let({ x: Num(1) }, Var("x")), { "x": Num(2) }, Num(1))

console.log('\n☯︎ Lamda abstraction')
check(Lam("x", Num(1)), {}, Lam("x", Num(1)))
check(Lam("x", Var("y")), {}, Err("Undefined variable: y"))
check(Lam("x", Var("x")), {}, Lam("x", Var("x")))

console.log('\n☯︎ Application')
check(App(Num(1), Num(2)), {}, Err("Not a function: 1"))
check(App(Var("f"), Var("x")), {}, Err("Undefined variable: f"))
check(App(Var("f"), Var("x")), { f: Var("f") }, Err("Undefined variable: x"))
check(App(Var("f"), Var("x")), { f: Var("f"), x: Num(1) }, App(Var("f"), Num(1)))
check(App(Lam("x", Var("x")), Num(1)), {}, Num(1))
check(App(Lam("x", Var("x")), Var("x")), { x: Num(1) }, Num(1))
check(App(App(Var("f"), Num(1)), Var("x")), { f: Var("f"), x: Num(2) }, App(App(Var("f"), Num(1)), Num(2)))

console.log('\n☯︎ Addition')
check(Add(), {}, Add())
check(App(Add(), Var("x")), { x: Num(3) }, App(Add(), Num(3)))
check(add(Var("x"), Var("y")), { x: Num(3), y: Num(2) }, Num(5))

console.log('\n☯︎ Subtraction')
check(Sub(), {}, Sub())
check(App(Sub(), Var("x")), { x: Num(3) }, App(Sub(), Num(3)))
check(sub(Var("x"), Var("y")), { x: Num(3), y: Num(2) }, Num(1))

console.log('\n☯︎ Multiplication')
check(Mul(), {}, Mul())
check(App(Mul(), Var("x")), { x: Num(3) }, App(Mul(), Num(3)))
check(mul(Var("x"), Var("y")), { x: Num(3), y: Num(2) }, Num(6))

console.log('\n☯︎ Equality')
check(Eq(), {}, Eq())
check(App(Eq(), Var("x")), { x: Num(3) }, App(Eq(), Num(3)))
check(eq(Var("x"), Var("y")), { x: Num(1), y: Num(2) }, Lam("True", Lam("False", Var("False"))))
check(eq(Var("x"), Var("y")), { x: Num(2), y: Num(2) }, Lam("True", Lam("False", Var("True"))))
check(eq(Var("x"), Var("y")), { x: Num(3), y: Num(2) }, Lam("True", Lam("False", Var("False"))))

console.log('\n☯︎ Simple recursion')
check(Var("f"), { f: Lam("x", App(Var("f"), (Var("x")))) }, Lam("x", App(Var("f"), Var("x"))))
check(Var("f"), { f: Lam("x", App(Var("x"), (Var("f")))) }, Lam("x", App(Var("x"), Var("f"))))

console.log('\n☯︎ Factorial')
// f 0 = 1
// f n = n * f (n - 1)
const factorial = Lam("n", app(eq(Var("n"), Num(0)), [Num(1), mul(Var("n"), App(Var("f"), sub(Var("n"), Num(1))))]))
check(Var("f"), { f: factorial }, factorial)
check(App(Var("f"), Num(0)), { f: factorial }, Num(1))
check(App(Var("f"), Num(1)), { f: factorial }, Num(1))
check(App(Var("f"), Num(5)), { f: factorial }, Num(120))
