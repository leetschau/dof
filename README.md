# donfeng: donno in ML

donfeng is a reimplementation of [donno](https://github.com/leetschau/donno) (in Python),
while focuses on the following features:

* Domain modeling based on powerful type system provided by ML family languages.
  Python provides type hint. However the absence of [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type)
  makes Python not a good choice for complex application modeling;
* Cocurrent searching on multi-core processors of modern machines;
* Auto-release based on CI/CD functionality of Github:
  providing native binary for both Linux and Windows;
* Single-executable style deployment: using donno by downloading donno executable with `wget`
  and install `git` with package manager, that's all;
* *Real* cross-platform: donno in Python depends heavily on several Linux shell tools,
  which gives it good performance and concise implementation.
  It can run on Windows in WSL. However it's not *native* to Windows.
  With .NET SDK and F#, it's convenient to build native applications for both Linux and Windows.

## Todo list

* Configuration module
* Sync via git
