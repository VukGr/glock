# Glock
A minimalistic hourglass-style graphical clock application written in Racket.

## How it works
#![Demo Image](https://i.imgur.com/xUFuyZk.png)
Rather than focusing on being able to tell the exact time at a glance I went for a design that graphically shows how much time is left in a single day.
In the left hand side every block signifies an hour.
In the right hand side every block signifies a minute, while the current minute block gradually fills up.

Colors, along with the offset between the blocks, can be configured in the glock.json file.

## Why Racket
It was what I was learning at the time.
Though I prefer lisp-based languages, if I was making this now I'd probably go with C or C++ to lower memory usage (both RAM and binary).
However at 120mb of ram usage and 30mb binary size it isn't horrid enough to warrant a rewrite in my opinion, especially considering the amount of electron based apps out nowadays.
