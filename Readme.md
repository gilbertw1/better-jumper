# better-jumper

A configurable jump list implementation for Emacs.

<br />

## Usage

Better jumper exposes the following functions

| Command                     | Description                                                         |
|--------------------------   |---------------------------------------------------------------------|
| better-jumper-set-jump      | Adds a new jump location to jump list using current buffer/position |
| better-jumper-jump-backward | Jumps to back to previous location in jump list                     |
| better-jumper-jump-forward  | Jumps forward to next location in jump list                         |

Configure as a drop in replacement for `evil-jump`:

```
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward))
```

<br />

## Configuration Options

##### *Context* - `better-jumper-context`

This setting specifies the context in which jump lists are tracked. This can
either be set to `'buffer` or `'window`. If the value is `'buffer` then a jump
list is maintained for each individual buffer. Conversly, if the value is
`'window` then the jump list is maintained per window and will operate across
buffers in that window.

*NOTE* - `'buffer` context support is temporarily not working.

##### *New Window Behavior* - `better-jumper-new-window-behavior`

This setting specifies the behavior that will take place when a new window is
created AND the current context is set to `'window`. This can be either set to
`'copy` or `'empty`. If the value is `'copy` then the last selected window's
jump list will be copied to the new window. If the value is `'empty` then the
new window's jump list will start empty.

##### *Max Length* - `better-jumper-max-length`

This is a numeric value that dictate the maximum length that a jump list can
grow to. If the length of a jump list exceeds this size then the oldest items in
the list will be dropped.

##### *Use Evil Jump Advice* - `better-jumper-use-evil-jump-advice`

If non-nil better jumper will attach a piece of advice to the `evil-jump`
function that will ensure that anytime a jump is added using `evil-jump` a
corresponding jump will be added using `better-jumper`.

<br />

## Hooks

##### *Pre-jump Hook* - `better-jumper-pre-jump-hook`

##### *Post-jump Hook* - `better-jumper-post-jump-hook`

<br />

## Comparison with `evil-jump`

This package was heavily inspired by `evil-jump` and initially was planned as a
modification of or pull request to `evil`. It was primarily born out of the
desire to isolate jumps across `persp-mode` perspectives, however the changes
proved to be to large to be a simple modification. Additionally, this package
provides more customization options as well as a few other core improvements.

A few advantages of `better-jumper` are:

* Properly isolates jump lists between `persp-mode` perspectives and saves the
  jump lists to those perspectives.

* True buffer specific jump lists. When instructed to not cross buffer
  boundaries `evil-jumper` still tracks jumps per window only limits the jumps
  available to ones located in the current buffer.
  
* Configurable new window behavior. `evil-jumper` ALWAYS copies the jump list
  from the previously selected window to any newly created window.
