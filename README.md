# Functional Programming Foundations: Haskell Template & Learning Journey

Welcome to my personal **Haskell Exploration Repository**. This project serves as a practical, hands-on environment where I explored the foundational principles of pure functional programming (FP). I uploaded this codebase to my GitHub profile to demonstrate my active learning process, algorithmic thinking, and structural problem-solving.

---

## 💡 The Philosophy: Active Learning & Pragmatism

* **An Active Learning Journey:** This repository is a record of continuous growth and experimentation. It is a playground for learning concepts from scratch.
* **Intentionally Work-in-Progress:** Not every single file or edge-case is completely filled out or "production-ready". The focus of this repository is the journey of grasping pure FP paradigms, meaning some code sections might reflect different stages of my learning curve.
* **Concept over Perfection:** Rather than cloning boilerplate solutions, the goal was to manually break down how higher-order functions, recursion, and composition operate under the hood in Haskell.

---

## 📂 Repository Structure & Modules

The repository is structured logically into core mathematical and functional programming domains within the `src/` directory:

### 🧩 Functions
* **`Composition.hs`** – Mastering point-free style, function pipelines, and the dot (`.`) operator to build complex behaviors from simple units.
* **`Functions.hs`** – Custom implementations of standard functional blocks, currying, and partial function application.
* **`Operators.hs`** – Understanding custom infix/prefix operations, fixity declarations, and precedence rules.

### 📜 Lists
* **`Comprehension.hs`** – Utilizing Haskell’s expressive list comprehensions for filtering, mapping, and Cartesian products.
* **`ConcatMap.hs`** – Diving into flattening and monadic-like structures using custom concat-map utilities.
* **`Foldl.hs` / `Foldr.hs`** – Studying the architectural and performance differences between left fold (tail-recursive, eager/lazy variants) and right fold (lazy, works on infinite lists).
* **`MapFilter.hs`** – Re-implementing the core pillars of list transformation from first principles.
* **`Recursion.hs`** / **`ListsFak`** – Writing explicit recursive functions, structural induction, and working with mathematical concepts (like factorials) using accumulator patterns.

---

## 🛠️ Ecosystem & Tooling

This project uses the modern Haskell ecosystem for building, testing, and dependency management:
* **Stack (`stack.yaml` / `.stack-work`)** – Used as the primary build tool to manage reproducible compiler (GHC) versions and isolated sandboxes.
* **Hpack (`package.yaml` / `.cabal`)** – Utilizes a clean `package.yaml` specification which dynamically generates the underlying Cabal infrastructure.
* **GHCi (`GHCi.md`)** – Includes an interactive REPL playground environment workflow for rapid prototyping and expression evaluation.
* **CI/CD (`.gitlab-ci.yml`)** – Contains configured pipeline configurations for automated code validation.

---

## 🧠 Core Paradigms Demonstrated

By working through this template, I have deeply internalized the following software engineering paradigms:

1. **Immutability & Purity:** Total absence of side effects. Functions always yield the exact same output for the same input, vastly increasing testability and reasoning.
2. **Lazy Evaluation:** Evaluating expressions only when their results are explicitly demanded, allowing for powerful concepts like processing infinite data structures.
3. **Type Safety & Type Inference:** Utilizing GHC's powerful static type system to catch logical flaws at compile time rather than runtime.
4. **Declarative Style:** Writing code that describes *what* to solve rather than *how* to step-by-step manipulate CPU memory registers.

---

*Feel free to browse through the modules to review my implementations and see how I approach pure functional logic!*
