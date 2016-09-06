# reveal-hs
Write your Reveal.js presentations in Haskell!

## Demo

![Demo](reveal-hs-demo.png)


## Current status

This project is far from being complete. Currently only markdown quasiquoter is implemented.


## Usage

With reveal-hs, each presentation is a stack project.

* Add reveal-hs to your `extra-deps`.
* Write your slides.
* `stack clean; stack build;  stack exec reveal-hs-exe > output.html`.
* That's it! Open `output.html` with your favorite browser and check your work!

Note: 

1. reveal-js needs Template Haskell to work so as to reduce some boilerplate when writing slides.
But to ensure all slide groups (haskell modules) are built into the final executable, you will
have to do `stack clean` prior to build, otherwise you will risk missing some (if not all) 
slides from the output.
1. Make sure to import all slide modules from the main module in the order they occur.
