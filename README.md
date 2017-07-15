# flycheck-pbxproj

Adds syntax checking capabilities to flycheck for Xcode projects
(.pbxproj) using Kin (https://github.com/Karumi/Kin).

Requires pbxproj-mode, a major mode for editing Xcode project files
(https://github.com/danielmartin/pbxproj-mode).

Do you hate when you solve an Xcode project merge conflict incorrectly
and now you cannot open it in Xcode? Here's flycheck-pbxproj in Ediff
helping you prevent this before saving the file:

![flycheck-pbxproj in action inside Ediff](https://raw.githubusercontent.com/danielmartin/flycheck-pbxproj/master/Screenshot.png)

This is a work in progress that would probably require some
performance improvements in Kin for "on the fly" syntax checks.

## Install

### MELPA

Not Available Yet

### Manual

* Download flycheck-pbxproj from https://github.com/danielmartin/flycheck-pbxproj
* Put the repo in some directory like `~/.emacs.d/vendor/`
* Add this code into your Emacs configuration:

```
(require 'flycheck-pbxproj)
(flycheck-pbxproj-setup)
```
