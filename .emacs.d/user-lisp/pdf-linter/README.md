# pdf-linter

Lint PDF documents from Emacs using PDFBox Preflight
(https://pdfbox.apache.org/download.cgi)

![pdf-linter in action](https://raw.githubusercontent.com/danielmartin/pdf-linter/master/Screenshot.png)

This is a work in progress.

## Install

### MELPA

Not Available Yet

### Manual

* Download PDFBox Preflight from https://pdfbox.apache.org/download.cgi
* Download pdf-linter from https://github.com/danielmartin/pdf-linter
* Put the repo in some directory like `~/.emacs.d/vendor/`
* Add this code into your Emacs configuration:

```
(require 'pdf-linter)
(setq pdf-linter-jar "path/to/pdfbox/preflight.jar")
```

From a buffer visiting a PDF document, run this:

```
M-x pdf-lint
```

Note: You may need to set the `JAVA_HOME` environment variable
correctly and configure Emacs to import it.
