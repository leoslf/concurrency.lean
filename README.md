# Concurrency for Lean 4

A minimal concurrency library for Lean 4 featuring Haskell-style `MVar` and counting semaphores.

This project implements core concurrency primitives in pure Lean 4 as a foundation for safe synchronization and shared-state communication between threads.

## Features

- **`MVar`** — mutable synchronization variable modeled after Haskell's `MVar`
- **`Semaphore`** — a basic counting semaphore for resource management and thread coordination

