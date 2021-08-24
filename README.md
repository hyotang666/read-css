# READ-CSS 2.0.0
## What is this?
CSS Reader.

## Alternatives.
Please tell me if exists.

## Usage

```lisp
(with-open-file (s "path/to/css.css")
  (read-css s))
```
For details, see [spec file](spec/read-css.lisp).

### To serialize.

```lisp
* (defvar *style* (read-style-from-string "p { width: 100px; }"))
*STYLE*

* *style*
#S(QUALIFIED-RULE
   :SELECTORS ("p")
   :DECLARATIONS (#S(CSS-DECLARATION
                     :NAME "width"
		     :IMPORTANTP NIL
		     :LIST ((#S(DIMENSION-TOKEN :VALUE 100 :UNIT "px"))))))

* (tagbody (princ *style*))
p { width:100px; }
NIL
```
*NOTE:* `PRINC` does not emit newline.
If you want to `PRINC` some styles, you need to `TERPRI`.

### To minimize.
Serialization heavily depends on common lisp pretty printing features,
so if `*PRINT-PRETTY*` is `NIL` indentation is not printed.

```lisp
* (let ((*print-pretty* nil))
    (princ style))
```

## From developer
Alpha quality.

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.1.7
* CCL-BIN/1.12.1
* ECL/21.2.1
* CLISP/2.49

## Installation

