![releases](https://img.shields.io/github/tag/jwintz/prolusion.svg)
![issues](https://img.shields.io/github/issues/jwintz/prolusion.svg)
![license](https://img.shields.io/github/license/jwintz/prolusion.svg)
![built](https://img.shields.io/badge/built%20with-Prolusion-6f62ba.svg)

***

# Prolusion

From Latin prolusio(n- ), from prolus- ‘practised beforehand’, from
the verb proludere, from pro ‘before’ + ludere ‘to play’.

Prolusion is meant to be as light a possible, and is completely
tweaked towards my own use of emacs. For those who are looking for a
more polished configuration kit, I can only recommand
[Prelude](https://github.com/bbatsov/prelude) or
[Spacemacs](https://github.com/syl20bnr/spacemacs).

Prolusion is organized as a set of layers, each of which is organized as
follows: requirements, setup, functions, hooks, modeline and
keybindings.

- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Fonts](#fonts)
- [Irony](#irony)
- [Upgrade](#upgrade)
- [Keybindings](#keybindings)

## Prerequisites

    $ brew tap d12frosted/emacs-plus
    $ brew install emacs-plus --HEAD --with-natural-title-bar
    $ brew install global --with-pygments --with-ctags
    $ brew install cmake
    $ brew install llvm --with-clang
    $ brew install ack
    $ brew install the_silver_searcher
    $ brew install mu --with-emacs --HEAD
    $ brew install isync
    $ brew install msmtp
    $ brew install nodejs npm
    $ npm install -g tern tern-lint
    $ npm install -g eslint babel-eslint eslint-plugin-react
    $ ln -s $/.prolusion.d/prolusion/shell/eslintrc ~/.eslintrc

## Installation

    $ curl -L https://raw.github.com/jwintz/prolusion/master/prolusion-installer/prolusion-installer.sh | sh

## Upgrade

**Emacs**

    $ brew   install emacs-plus --HEAD --with-natural-title-bar # if emacs is not installed
    $ brew reinstall emacs-plus --HEAD --with-natural-title-bar # if emacs is     installed

**Emacs prolusion**

    M-x prolusion/upgrade

**Emacs prolusion packages**

    M-x prolusion/upgrade-packages

## Fonts

Take care to install all the fonts provided in `.prolusion/prolusion-fonts`.

Should you be on Linux, use:

    $ install -m 0644 -D ~/.emacs.d/prolusion-fonts/*.ttf -t ~/.local/share/fonts/

or:

    M-x all-the-icons-install-fonts

## Irony

On MacOSX, it is mandatory to add `-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON` to the `M-x irony-install-server` command.

## Tern

To get javscript completion, add the following `.tern-project` taht contains at least the following minial configuration.

```
{
    "plugins": {
        "node": {
        }
    }
}
```

## Keybindings

**prolusion-packages**

| Keybinding           | Function                     |
| -------------------- |:---------------------------- |
| <kbd>`C-c u u`</kbd> | `prolusion/upgrade`          |
| <kbd>`C-c u p`</kbd> | `prolusion/upgrade-packages` |

**prolusion-behavior**

| Keybinding         | Function          |
| ------------------ |:----------------- |
| <kbd>`C-x o`</kbd> | `other-window`    |
| <kbd>`C-x O`</kbd> | `other-window -1` |

| Keybinding           | Function              |
| -------------------- |:--------------------- |
| <kbd>`C-+`</kbd>     | `text-scale-increase` |
| <kbd>`C--`</kbd>     | `text-scale-decrease` |
| <kbd>`C-x C-0`</kbd> | `text-scale-adjust`   |

| Keybinding         | Function              |
| ------------------ | ---------------------:|
| <kbd>`C-x (`</kbd> | `start-kbd-macro`     |
| <kbd>`C-x )`</kbd> | `end-kbd-macro`       |
| <kbd>`C-x e`</kbd> | `call-last-kbd-macro` |

| Keybinding         | Function        |
| ------------------ | ---------------:|
| <kbd>`C-M-f`</kbd> | `forward-sexp`  |
| <kbd>`C-M-b`</kbd> | `backward-sexp` |

| Keybinding           | Function           |
| -------------------- | ------------------:|
| <kbd>`C-x n n`</kbd> | `narrow-to-region` |
| <kbd>`C-x n w`</kbd> | `widen`            |

| Keybinding         | Function         |
| ------------------ | ----------------:|
| <kbd>`M-SPC`</kbd> | `just-one-space` |

| Keybinding                                | Function                               |
| ----------------------------------------- |:-------------------------------------- |
| <kbd>M-x resize-window</kbd> <kbd>n</kbd> | `resize-window--enlarge-down`          |
| <kbd>M-x resize-window</kbd> <kbd>p</kbd> | `resize-window--enlarge-up`            |
| <kbd>M-x resize-window</kbd> <kbd>f</kbd> | `resize-window--enlarge-horizontally`  |
| <kbd>M-x resize-window</kbd> <kbd>b</kbd> | `resize-window--shrink-horizontally`   |
| <kbd>M-x resize-window</kbd> <kbd>r</kbd> | `resize-window--reset-windows`         |
| <kbd>M-x resize-window</kbd> <kbd>w</kbd> | `resize-window--cycle-window-positive` |
| <kbd>M-x resize-window</kbd> <kbd>W</kbd> | `resize-window--cycle-window-negative` |

| Keybinding      | Function         |
| --------------- |:---------------- |
| <kbd>`F7`</kbd> | `neotree-toggle` |
| <kbd>`F8`</kbd> | `nlinum-mode`    |

**prolusion-builtins**

| Keybinding           | Function                       |
| -------------------- | ------------------------------:|
| <kbd>`C-c b c`</kbd> |              `clear-rectangle` |
| <kbd>`C-c b d`</kbd> |             `delete-rectangle` |
| <kbd>`C-c b k`</kbd> |               `kill-rectangle` |
| <kbd>`C-c b o`</kbd> |               `open-rectangle` |
| <kbd>`C-c b t`</kbd> |             `string-rectangle` |
| <kbd>`C-c b y`</kbd> |               `yank-rectangle` |
| <kbd>`C-c b w`</kbd> | `wdired-change-to-wdired-mode` |
| <kbd>`C-c b s`</kbd> |                 `bookmark-set` |
| <kbd>`C-c b j`</kbd> |                `bookmark-jump` |
| <kbd>`C-c b l`</kbd> |          `bookmark-bmenu-list` |
| <kbd>`C-h k`</kbd>   |                 `describe-key` |
| <kbd>`C-h f`</kbd>   |            `describe-function` |
| <kbd>`C-h v`</kbd>   |            `describe-variable` |
| <kbd>`C-h b`</kbd>   |            `describe-bindings` |
| <kbd>`C-h a`</kbd>   |              `apropos-command` |
| <kbd>`M-u`</kbd>     |                  `upcase-word` |
| <kbd>`M-l`</kbd>     |                `downcase-word` |
| <kbd>`M-c`</kbd>     |              `capitalize-word` |

**prolusion-eshell**

| Keybinding           | Function                        |
| -------------------- |:------------------------------- |
| <kbd>`C-c l l`</kbd> | `mutli-eshell`                  |
| <kbd>`C-c l o`</kbd> | `mutli-eshell-switch`           |
| <kbd>`C-c l 0`</kbd> | `mutli-eshell-go-back`          |
| <kbd>`C-c l c`</kbd> | `prolusion/eshell-clear-buffer` |

**prolusion-editor**

| Keybinding           | Function                   |
| -------------------- |:-------------------------- |
| <kbd>`C-c e s`</kbd> | `ff-find-other-file`       |
| <kbd>`C-c e m`</kbd> | `make-header`              |
| <kbd>`C-c e c`</kbd> | `make-box-comment`         |
| <kbd>`C-c e d`</kbd> | `make-divider`             |
| <kbd>`C-c e r`</kbd> | `make-revision`            |
| <kbd>`C-c e g`</kbd> | `update-file-header`       |
| <kbd>`C-c e l`</kbd> | `prolusion/duplicate-line` |
| <kbd>`C-c e e`</kbd> | `iedit-mode`               |
| <kbd>`C-c e f`</kbd> | `helm-mini`                |
| <kbd>`C-c e b`</kbd> | `helm-buffers-list`        |
| <kbd>`C-c e k`</kbd> | `helm-show-kill-ring`      |

**prolusion-modes**

| Keybinding             | Function               |
| ---------------------- |:---------------------- |
| <kbd>`C-c m p a`</kbd> | `conda-env-activate`   |
| <kbd>`C-c m p d`</kbd> | `conda-env-deactivate` |
| <kbd>`C-c m p l`</kbd> | `conda-env-list`       |
| <kbd>`C-c m r a`</kbd> | `global-rbenv-mode`    |

**prolusion-snippets**

| Keybinding           | Function                 |
| -------------------- |:------------------------ |
| <kbd>`C-c y n`</kbd> | `yas-new-snippet`        |
| <kbd>`C-c y s`</kbd> | `yas-insert-snippet`     |
| <kbd>`C-c y v`</kbd> | `yas-visit-snippet-file` |

**prolusion-vc**

| Keybinding           | Function       |
| -------------------- |:-------------- |
| <kbd>`C-c v m`</kbd> | `magit-status` |

**prolusion-projectile**

| Keybinding           | Function                                      |
| -------------------- |:--------------------------------------------- |
| <kbd>C-c p h</kbd>   | `helm-projectile`                             |
| <kbd>C-c p a</kbd>   | `helm-projectile-find-other-file`             |
| <kbd>C-c p f</kbd>   | `helm-projectile-find-file`                   |
| <kbd>C-c p F</kbd>   | `helm-projectile-find-file-in-known-projects` |
| <kbd>C-c p g</kbd>   | `helm-projectile-find-file-dwim`              |
| <kbd>C-c p d</kbd>   | `helm-projectile-find-dir`                    |
| <kbd>C-c p p</kbd>   | `helm-projectile-switch-project`              |
| <kbd>C-c p e</kbd>   | `helm-projectile-recentf`                     |
| <kbd>C-c p b</kbd>   | `helm-projectile-switch-to-buffer`            |
| <kbd>C-c p R</kbd>   | `projectile-regenerate-tags`                  |
| <kbd>C-c p j</kbd>   | `projectile-projectile-find-tag`              |
| <kbd>C-c p s g</kbd> | `helm-projectile-grep`                        |
| <kbd>C-c p s a</kbd> | `helm-projectile-ack`                         |
| <kbd>C-c p s s</kbd> | `helm-projectile-ag`                          |

**prolusion-completion**

| Keybinding         | Function                         |
| ------------------ |:-------------------------------- |
| <kbd>C-c c c</kbd> | `anaconda-mode-complete`         |
| <kbd>C-c c d</kbd> | `anaconda-mode-find-definitions` |
| <kbd>C-c c a</kbd> | `anaconda-mode-find-assignments` |
| <kbd>C-c c r</kbd> | `anaconda-mode-find-references`  |
| <kbd>C-c c b</kbd> | `anaconda-mode-go-back`          |
| <kbd>C-c c s</kbd> | `anaconda-mode-show-doc`         |

**prolusion-checking**

| Keybinding           | Function                   |
| -------------------- |:-------------------------- |
| <kbd>C-c ! l</kbd>   | `flycheck-list-errors`     |
| <kbd>C-c ! n</kbd>   | `flycheck-next-error`      |
| <kbd>C-c ! p</kbd>   | `flycheck-previous-error`  |
| <kbd>C-c ! s</kbd>   | `flycheck-select-checker`  |
| <kbd>C-c ! x</kbd>   | `flycheck-disable-checker` |

**prolusion-workspaces**

| Keybinding         | Function                           |
| ------------------ |:---------------------------------- |
| <kbd>C-c w n</kbd> | `persp-next`                       |
| <kbd>C-c w p</kbd> | `persp-prev`                       |
| <kbd>C-c w s</kbd> | `persp-frame-switch`               |
| <kbd>C-c w S</kbd> | `persp-window-switch`              |
| <kbd>C-c w r</kbd> | `persp-rename`                     |
| <kbd>C-c w c</kbd> | `persp-kill`                       |
| <kbd>C-c w a</kbd> | `persp-add-buffer`                 |
| <kbd>C-c w t</kbd> | `persp-temporarily-display-buffer` |
| <kbd>C-c w i</kbd> | `persp-import-buffers`             |
| <kbd>C-c w k</kbd> | `persp-remove-buffer`              |
| <kbd>C-c w K</kbd> | `persp-kill-buffer`                |
| <kbd>C-c w w</kbd> | `persp-save-state-to-file`         |
| <kbd>C-c w l</kbd> | `persp-load-state-from-file`       |

**prolusion-neotree**

In `neotree` buffer:

| Keybinding         | Function                     |
| ------------------ |:---------------------------- |
| <kbd>n</kbd>       | `neotree-next-line`          |
| <kbd>p</kbd>       | `neotree-prev-line`          |
| <kbd>g</kbd>       | `neotree-refresh`            |
| <kbd>H</kbd>       | `neotree-hidden-file-toggle` |
| <kbd>C-c C-n</kbd> | `neotree-create-node`        |
| <kbd>C-c C-d</kbd> | `neotree-delete-node`        |
| <kbd>C-c C-r</kbd> | `neotree-rename-node`        |
| <kbd>C-c C-c</kbd> | `neotree-change-root`        |
