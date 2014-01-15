(in-package :mustache-test)
(defsuite spec-suite (mustache-suite))

(deftest |test-spec-00| (spec-suite)
 (let* ((template "Hello, {{lambda}}!")
        (data
         `((:lambda . ,(lambda () "world"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Hello, world!" (mustache-render-to-string template context)
    "Interpolation :: A lambda's return value should be interpolated." template
    data partials)))

(deftest |test-spec-01| (spec-suite)
 (let* ((template "Hello, {{lambda}}!")
        (data
         `((:planet . "world")
           (:lambda . ,(lambda () "{{planet}}"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Hello, world!" (mustache-render-to-string template context)
    "Interpolation - Expansion :: A lambda's return value should be parsed."
    template data partials)))

(deftest |test-spec-02| (spec-suite)
 (let* ((template
         "{{= | | =}}
Hello, (|&lambda|)!")
        (data
         `((:planet . "world")
           (:lambda . ,(lambda () "|planet| => {{planet}}"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Hello, (|planet| => world)!"
    (mustache-render-to-string template context)
    "Interpolation - Alternate Delimiters :: A lambda's return value should parse with the default delimiters."
    template data partials)))

(deftest |test-spec-03| (spec-suite)
 (let* ((template "{{lambda}} == {{{lambda}}} == {{lambda}}")
        (data
         `((:lambda . ,(let ((calls 0)) (lambda () (incf calls))))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "1 == 2 == 3" (mustache-render-to-string template context)
    "Interpolation - Multiple Calls :: Interpolated lambdas should not be cached."
    template data partials)))

(deftest |test-spec-04| (spec-suite)
 (let* ((template "<{{lambda}}{{{lambda}}}")
        (data
         `((:lambda . ,(lambda () ">"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "<&gt;>" (mustache-render-to-string template context)
    "Escaping :: Lambda results should be appropriately escaped." template data
    partials)))

(deftest |test-spec-05| (spec-suite)
 (let* ((template "<{{#lambda}}{{x}}{{/lambda}}>")
        (data
         `((:x . "Error!")
           (:lambda . ,(lambda (text) (if (equal text "{{x}}") "yes" "no")))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "<yes>" (mustache-render-to-string template context)
    "Section :: Lambdas used for sections should receive the raw section string."
    template data partials)))

(deftest |test-spec-06| (spec-suite)
 (let* ((template "<{{#lambda}}-{{/lambda}}>")
        (data
         `((:planet . "Earth")
           (:lambda . ,(lambda (text) (format nil "~A{{planet}}~A" text text)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "<-Earth->" (mustache-render-to-string template context)
    "Section - Expansion :: Lambdas used for sections should have their results parsed."
    template data partials)))

(deftest |test-spec-07| (spec-suite)
 (let* ((template "{{= | | =}}<|#lambda|-|/lambda|>")
        (data
         `((:planet . "Earth")
           (:lambda . ,(lambda (text) (format nil "~A{{planet}} => |planet|~A" text text)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "<-{{planet}} => Earth->"
    (mustache-render-to-string template context)
    "Section - Alternate Delimiters :: Lambdas used for sections should parse with the current delimiters."
    template data partials)))

(deftest |test-spec-08| (spec-suite)
 (let* ((template "{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}")
        (data
         `((:lambda . ,(lambda (text) (format nil "__~A__" text)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "__FILE__ != __LINE__"
    (mustache-render-to-string template context)
    "Section - Multiple Calls :: Lambdas used for sections should not be cached."
    template data partials)))

(deftest |test-spec-09| (spec-suite)
 (let* ((template "<{{^lambda}}{{static}}{{/lambda}}>")
        (data
         `((:static . "static")
           (:lambda . ,(lambda (text) nil))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "<>" (mustache-render-to-string template context)
    "Inverted Section :: Lambdas used for inverted sections should be considered truthy."
    template data partials)))

(deftest |test-spec-10| (spec-suite)
 (let* ((template "\"{{#boolean}}This should be rendered.{{/boolean}}\"")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"This should be rendered.\""
    (mustache-render-to-string template context)
    "Truthy :: Truthy sections should have their contents rendered." template
    data partials)))

(deftest |test-spec-11| (spec-suite)
 (let* ((template "\"{{#boolean}}This should not be rendered.{{/boolean}}\"")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "Falsey :: Falsey sections should have their contents omitted." template
    data partials)))

(deftest |test-spec-12| (spec-suite)
 (let* ((template "\"{{#context}}Hi {{name}}.{{/context}}\"")
        (data '((:context (:name . "Joe"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Hi Joe.\"" (mustache-render-to-string template context)
    "Context :: Objects and hashes should be pushed onto the context stack."
    template data partials)))

(deftest |test-spec-13| (spec-suite)
 (let* ((template
         "{{#a}}
{{one}}
{{#b}}
{{one}}{{two}}{{one}}
{{#c}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{#d}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{#e}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{/e}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{/d}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{/c}}
{{one}}{{two}}{{one}}
{{/b}}
{{one}}
{{/a}}
")
        (data
         '((:a (:one . 1)) (:b (:two . 2)) (:c (:three . 3)) (:d (:four . 4))
           (:e (:five . 5))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "1
121
12321
1234321
123454321
1234321
12321
121
1
"
    (mustache-render-to-string template context)
    "Deeply Nested Contexts :: All elements on the context stack should be accessible."
    template data partials)))

(deftest |test-spec-14| (spec-suite)
 (let* ((template "\"{{#list}}{{item}}{{/list}}\"")
        (data '((:list . #(((:item . 1)) ((:item . 2)) ((:item . 3))))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"123\"" (mustache-render-to-string template context)
    "List :: Lists should be iterated; list items should visit the context stack."
    template data partials)))

(deftest |test-spec-15| (spec-suite)
 (let* ((template "\"{{#list}}Yay lists!{{/list}}\"")
        (data '((:list . #())))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "Empty List :: Empty lists should behave like falsey values." template data
    partials)))

(deftest |test-spec-16| (spec-suite)
 (let* ((template
         "{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}
")
        (data '((:two . "second") (:bool . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "* first
* second
* third
"
    (mustache-render-to-string template context)
    "Doubled :: Multiple sections per template should be permitted." template
    data partials)))

(deftest |test-spec-17| (spec-suite)
 (let* ((template "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |")
        (data '((:bool . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| A B C D E |" (mustache-render-to-string template context)
    "Nested (Truthy) :: Nested truthy sections should have their contents rendered."
    template data partials)))

(deftest |test-spec-18| (spec-suite)
 (let* ((template "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |")
        (data '((:bool)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| A  E |" (mustache-render-to-string template context)
    "Nested (Falsey) :: Nested falsey sections should be omitted." template
    data partials)))

(deftest |test-spec-19| (spec-suite)
 (let* ((template "[{{#missing}}Found key 'missing'!{{/missing}}]")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[]" (mustache-render-to-string template context)
    "Context Misses :: Failed context lookups should be considered falsey."
    template data partials)))

(deftest |test-spec-20| (spec-suite)
 (let* ((template "\"{{#list}}({{.}}){{/list}}\"")
        (data '((:list . #("a" "b" "c" "d" "e"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"(a)(b)(c)(d)(e)\""
    (mustache-render-to-string template context)
    "Implicit Iterator - String :: Implicit iterators should directly interpolate strings."
    template data partials)))

(deftest |test-spec-21| (spec-suite)
 (let* ((template "\"{{#list}}({{.}}){{/list}}\"")
        (data '((:list . #(1 2 3 4 5))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"(1)(2)(3)(4)(5)\""
    (mustache-render-to-string template context)
    "Implicit Iterator - Integer :: Implicit iterators should cast integers to strings and interpolate."
    template data partials)))

(deftest |test-spec-22| (spec-suite)
 (let* ((template "\"{{#list}}({{.}}){{/list}}\"")
        (data '((:list . #(1.1 2.2 3.3 4.4 5.5))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"(1.1)(2.2)(3.3)(4.4)(5.5)\""
    (mustache-render-to-string template context)
    "Implicit Iterator - Decimal :: Implicit iterators should cast decimals to strings and interpolate."
    template data partials)))

(deftest |test-spec-23| (spec-suite)
 (let* ((template "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\"")
        (data '((:a (:b (:c . t)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Here\" == \"Here\""
    (mustache-render-to-string template context)
    "Dotted Names - Truthy :: Dotted names should be valid for Section tags."
    template data partials)))

(deftest |test-spec-24| (spec-suite)
 (let* ((template "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"")
        (data '((:a (:b (:c)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\" == \"\"" (mustache-render-to-string template context)
    "Dotted Names - Falsey :: Dotted names should be valid for Section tags."
    template data partials)))

(deftest |test-spec-25| (spec-suite)
 (let* ((template "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"")
        (data '((:a)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\" == \"\"" (mustache-render-to-string template context)
    "Dotted Names - Broken Chains :: Dotted names that cannot be resolved should be considered falsey."
    template data partials)))

(deftest |test-spec-26| (spec-suite)
 (let* ((template
         " | {{#boolean}}	|	{{/boolean}} | 
")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " | 	|	 | 
"
    (mustache-render-to-string template context)
    "Surrounding Whitespace :: Sections should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-27| (spec-suite)
 (let* ((template
         " | {{#boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " |  
  | 
"
    (mustache-render-to-string template context)
    "Internal Whitespace :: Sections should not alter internal whitespace."
    template data partials)))

(deftest |test-spec-28| (spec-suite)
 (let* ((template
         " {{#boolean}}YES{{/boolean}}
 {{#boolean}}GOOD{{/boolean}}
")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " YES
 GOOD
"
    (mustache-render-to-string template context)
    "Indented Inline Sections :: Single-line sections should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-29| (spec-suite)
 (let* ((template
         "| This Is
{{#boolean}}
|
{{/boolean}}
| A Line
")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| This Is
|
| A Line
"
    (mustache-render-to-string template context)
    "Standalone Lines :: Standalone lines should be removed from the template."
    template data partials)))

(deftest |test-spec-30| (spec-suite)
 (let* ((template
         "| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line
")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| This Is
|
| A Line
"
    (mustache-render-to-string template context)
    "Indented Standalone Lines :: Indented standalone lines should be removed from the template."
    template data partials)))

(deftest |test-spec-31| (spec-suite)
 (let* ((template
         "|
{{#boolean}}
{{/boolean}}
|")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|
|"
    (mustache-render-to-string template context)
    "Standalone Line Endings :: \"\\r\\n\" should be considered a newline for standalone tags."
    template data partials)))

(deftest |test-spec-32| (spec-suite)
 (let* ((template
         "  {{#boolean}}
#{{/boolean}}
/")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "#
/"
    (mustache-render-to-string template context)
    "Standalone Without Previous Line :: Standalone tags should not require a newline to precede them."
    template data partials)))

(deftest |test-spec-33| (spec-suite)
 (let* ((template
         "#{{#boolean}}
/
  {{/boolean}}")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "#
/
"
    (mustache-render-to-string template context)
    "Standalone Without Newline :: Standalone tags should not require a newline to follow them."
    template data partials)))

(deftest |test-spec-34| (spec-suite)
 (let* ((template "|{{# boolean }}={{/ boolean }}|")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|=|" (mustache-render-to-string template context)
    "Padding :: Superfluous in-tag whitespace should be ignored." template data
    partials)))

(deftest |test-spec-35| (spec-suite)
 (let* ((template "\"{{>text}}\"")
        (data 'nil)
        (partials '((:text . "from partial")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"from partial\""
    (mustache-render-to-string template context)
    "Basic Behavior :: The greater-than operator should expand to the named partial."
    template data partials)))

(deftest |test-spec-36| (spec-suite)
 (let* ((template "\"{{>text}}\"")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "Failed Lookup :: The empty string should be used when the named partial is not found."
    template data partials)))

(deftest |test-spec-37| (spec-suite)
 (let* ((template "\"{{>partial}}\"")
        (data '((:text . "content")))
        (partials '((:partial . "*{{text}}*")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"*content*\"" (mustache-render-to-string template context)
    "Context :: The greater-than operator should operate within the current context."
    template data partials)))

(deftest |test-spec-38| (spec-suite)
 (let* ((template "{{>node}}")
        (data
         '((:content . "X") (:nodes . #(((:content . "Y") (:nodes . #()))))))
        (partials '((:node . "{{content}}<{{#nodes}}{{>node}}{{/nodes}}>")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "X<Y<>>" (mustache-render-to-string template context)
    "Recursion :: The greater-than operator should properly recurse." template
    data partials)))

(deftest |test-spec-39| (spec-suite)
 (let* ((template "| {{>partial}} |")
        (data 'nil)
        (partials '((:partial . "	|	")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| 	|	 |" (mustache-render-to-string template context)
    "Surrounding Whitespace :: The greater-than operator should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-40| (spec-suite)
 (let* ((template
         "  {{data}}  {{> partial}}
")
        (data '((:data . "|")))
        (partials
         '((:partial . ">
>")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  |  >
>
"
    (mustache-render-to-string template context)
    "Inline Indentation :: Whitespace should be left untouched." template data
    partials)))

(deftest |test-spec-41| (spec-suite)
 (let* ((template
         "|
{{>partial}}
|")
        (data 'nil)
        (partials '((:partial . ">")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|
>|"
    (mustache-render-to-string template context)
    "Standalone Line Endings :: \"\\r\\n\" should be considered a newline for standalone tags."
    template data partials)))

(deftest |test-spec-42| (spec-suite)
 (let* ((template
         "  {{>partial}}
>")
        (data 'nil)
        (partials
         '((:partial . ">
>")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  >
  >>"
    (mustache-render-to-string template context)
    "Standalone Without Previous Line :: Standalone tags should not require a newline to precede them."
    template data partials)))

(deftest |test-spec-43| (spec-suite)
 (let* ((template
         ">
  {{>partial}}")
        (data 'nil)
        (partials
         '((:partial . ">
>")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal ">
  >
  >"
    (mustache-render-to-string template context)
    "Standalone Without Newline :: Standalone tags should not require a newline to follow them."
    template data partials)))

(deftest |test-spec-44| (spec-suite)
 (let* ((template
         "\\
 {{>partial}}
/
")
        (data
         '((:content . "<
->")))
        (partials
         '((:partial . "|
{{{content}}}
|
")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\\
 |
 <
->
 |
/
"
    (mustache-render-to-string template context)
    "Standalone Indentation :: Each line of the partial should be indented before rendering."
    template data partials)))

(deftest |test-spec-45| (spec-suite)
 (let* ((template "|{{> partial }}|")
        (data '((:boolean . t)))
        (partials '((:partial . "[]")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|[]|" (mustache-render-to-string template context)
    "Padding Whitespace :: Superfluous in-tag whitespace should be ignored."
    template data partials)))

(deftest |test-spec-46| (spec-suite)
 (let* ((template "\"{{^boolean}}This should be rendered.{{/boolean}}\"")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"This should be rendered.\""
    (mustache-render-to-string template context)
    "Falsey :: Falsey sections should have their contents rendered." template
    data partials)))

(deftest |test-spec-47| (spec-suite)
 (let* ((template "\"{{^boolean}}This should not be rendered.{{/boolean}}\"")
        (data '((:boolean . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "Truthy :: Truthy sections should have their contents omitted." template
    data partials)))

(deftest |test-spec-48| (spec-suite)
 (let* ((template "\"{{^context}}Hi {{name}}.{{/context}}\"")
        (data '((:context (:name . "Joe"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "Context :: Objects and hashes should behave like truthy values." template
    data partials)))

(deftest |test-spec-49| (spec-suite)
 (let* ((template "\"{{^list}}{{n}}{{/list}}\"")
        (data '((:list . #(((:n . 1)) ((:n . 2)) ((:n . 3))))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\"" (mustache-render-to-string template context)
    "List :: Lists should behave like truthy values." template data partials)))

(deftest |test-spec-50| (spec-suite)
 (let* ((template "\"{{^list}}Yay lists!{{/list}}\"")
        (data '((:list . #())))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Yay lists!\"" (mustache-render-to-string template context)
    "Empty List :: Empty lists should behave like falsey values." template data
    partials)))

(deftest |test-spec-51| (spec-suite)
 (let* ((template
         "{{^bool}}
* first
{{/bool}}
* {{two}}
{{^bool}}
* third
{{/bool}}
")
        (data '((:two . "second") (:bool)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "* first
* second
* third
"
    (mustache-render-to-string template context)
    "Doubled :: Multiple inverted sections per template should be permitted."
    template data partials)))

(deftest |test-spec-52| (spec-suite)
 (let* ((template "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |")
        (data '((:bool)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| A B C D E |" (mustache-render-to-string template context)
    "Nested (Falsey) :: Nested falsey sections should have their contents rendered."
    template data partials)))

(deftest |test-spec-53| (spec-suite)
 (let* ((template "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |")
        (data '((:bool . t)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| A  E |" (mustache-render-to-string template context)
    "Nested (Truthy) :: Nested truthy sections should be omitted." template
    data partials)))

(deftest |test-spec-54| (spec-suite)
 (let* ((template "[{{^missing}}Cannot find key 'missing'!{{/missing}}]")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[Cannot find key 'missing'!]"
    (mustache-render-to-string template context)
    "Context Misses :: Failed context lookups should be considered falsey."
    template data partials)))

(deftest |test-spec-55| (spec-suite)
 (let* ((template "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\"")
        (data '((:a (:b (:c . t)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\" == \"\"" (mustache-render-to-string template context)
    "Dotted Names - Truthy :: Dotted names should be valid for Inverted Section tags."
    template data partials)))

(deftest |test-spec-56| (spec-suite)
 (let* ((template "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\"")
        (data '((:a (:b (:c)))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Not Here\" == \"Not Here\""
    (mustache-render-to-string template context)
    "Dotted Names - Falsey :: Dotted names should be valid for Inverted Section tags."
    template data partials)))

(deftest |test-spec-57| (spec-suite)
 (let* ((template "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\"")
        (data '((:a)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Not Here\" == \"Not Here\""
    (mustache-render-to-string template context)
    "Dotted Names - Broken Chains :: Dotted names that cannot be resolved should be considered falsey."
    template data partials)))

(deftest |test-spec-58| (spec-suite)
 (let* ((template
         " | {{^boolean}}	|	{{/boolean}} | 
")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " | 	|	 | 
"
    (mustache-render-to-string template context)
    "Surrounding Whitespace :: Inverted sections should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-59| (spec-suite)
 (let* ((template
         " | {{^boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " |  
  | 
"
    (mustache-render-to-string template context)
    "Internal Whitespace :: Inverted should not alter internal whitespace."
    template data partials)))

(deftest |test-spec-60| (spec-suite)
 (let* ((template
         " {{^boolean}}NO{{/boolean}}
 {{^boolean}}WAY{{/boolean}}
")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " NO
 WAY
"
    (mustache-render-to-string template context)
    "Indented Inline Sections :: Single-line sections should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-61| (spec-suite)
 (let* ((template
         "| This Is
{{^boolean}}
|
{{/boolean}}
| A Line
")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| This Is
|
| A Line
"
    (mustache-render-to-string template context)
    "Standalone Lines :: Standalone lines should be removed from the template."
    template data partials)))

(deftest |test-spec-62| (spec-suite)
 (let* ((template
         "| This Is
  {{^boolean}}
|
  {{/boolean}}
| A Line
")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| This Is
|
| A Line
"
    (mustache-render-to-string template context)
    "Standalone Indented Lines :: Standalone indented lines should be removed from the template."
    template data partials)))

(deftest |test-spec-63| (spec-suite)
 (let* ((template
         "|
{{^boolean}}
{{/boolean}}
|")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|
|"
    (mustache-render-to-string template context)
    "Standalone Line Endings :: \"\\r\\n\" should be considered a newline for standalone tags."
    template data partials)))

(deftest |test-spec-64| (spec-suite)
 (let* ((template
         "  {{^boolean}}
^{{/boolean}}
/")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "^
/"
    (mustache-render-to-string template context)
    "Standalone Without Previous Line :: Standalone tags should not require a newline to precede them."
    template data partials)))

(deftest |test-spec-65| (spec-suite)
 (let* ((template
         "^{{^boolean}}
/
  {{/boolean}}")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "^
/
"
    (mustache-render-to-string template context)
    "Standalone Without Newline :: Standalone tags should not require a newline to follow them."
    template data partials)))

(deftest |test-spec-66| (spec-suite)
 (let* ((template "|{{^ boolean }}={{/ boolean }}|")
        (data '((:boolean)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|=|" (mustache-render-to-string template context)
    "Padding :: Superfluous in-tag whitespace should be ignored." template data
    partials)))

(deftest |test-spec-67| (spec-suite)
 (let* ((template
         "Hello from {Mustache}!
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Hello from {Mustache}!
"
    (mustache-render-to-string template context)
    "No Interpolation :: Mustache-free templates should render as-is." template
    data partials)))

(deftest |test-spec-68| (spec-suite)
 (let* ((template
         "Hello, {{subject}}!
")
        (data '((:subject . "world")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Hello, world!
"
    (mustache-render-to-string template context)
    "Basic Interpolation :: Unadorned tags should interpolate content into the template."
    template data partials)))

(deftest |test-spec-69| (spec-suite)
 (let* ((template
         "These characters should be HTML escaped: {{forbidden}}
")
        (data '((:forbidden . "& \" < >")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal
    "These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"
    (mustache-render-to-string template context)
    "HTML Escaping :: Basic interpolation should be HTML escaped." template
    data partials)))

(deftest |test-spec-70| (spec-suite)
 (let* ((template
         "These characters should not be HTML escaped: {{{forbidden}}}
")
        (data '((:forbidden . "& \" < >")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "These characters should not be HTML escaped: & \" < >
"
    (mustache-render-to-string template context)
    "Triple Mustache :: Triple mustaches should interpolate without HTML escaping."
    template data partials)))

(deftest |test-spec-71| (spec-suite)
 (let* ((template
         "These characters should not be HTML escaped: {{&forbidden}}
")
        (data '((:forbidden . "& \" < >")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "These characters should not be HTML escaped: & \" < >
"
    (mustache-render-to-string template context)
    "Ampersand :: Ampersand should interpolate without HTML escaping." template
    data partials)))

(deftest |test-spec-72| (spec-suite)
 (let* ((template "\"{{mph}} miles an hour!\"")
        (data '((:mph . 85)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"85 miles an hour!\""
    (mustache-render-to-string template context)
    "Basic Integer Interpolation :: Integers should interpolate seamlessly."
    template data partials)))

(deftest |test-spec-73| (spec-suite)
 (let* ((template "\"{{{mph}}} miles an hour!\"")
        (data '((:mph . 85)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"85 miles an hour!\""
    (mustache-render-to-string template context)
    "Triple Mustache Integer Interpolation :: Integers should interpolate seamlessly."
    template data partials)))

(deftest |test-spec-74| (spec-suite)
 (let* ((template "\"{{&mph}} miles an hour!\"")
        (data '((:mph . 85)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"85 miles an hour!\""
    (mustache-render-to-string template context)
    "Ampersand Integer Interpolation :: Integers should interpolate seamlessly."
    template data partials)))

(deftest |test-spec-75| (spec-suite)
 (let* ((template "\"{{power}} jiggawatts!\"")
        (data '((:power . 1.21)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"1.21 jiggawatts!\""
    (mustache-render-to-string template context)
    "Basic Decimal Interpolation :: Decimals should interpolate seamlessly with proper significance."
    template data partials)))

(deftest |test-spec-76| (spec-suite)
 (let* ((template "\"{{{power}}} jiggawatts!\"")
        (data '((:power . 1.21)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"1.21 jiggawatts!\""
    (mustache-render-to-string template context)
    "Triple Mustache Decimal Interpolation :: Decimals should interpolate seamlessly with proper significance."
    template data partials)))

(deftest |test-spec-77| (spec-suite)
 (let* ((template "\"{{&power}} jiggawatts!\"")
        (data '((:power . 1.21)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"1.21 jiggawatts!\""
    (mustache-render-to-string template context)
    "Ampersand Decimal Interpolation :: Decimals should interpolate seamlessly with proper significance."
    template data partials)))

(deftest |test-spec-78| (spec-suite)
 (let* ((template "I ({{cannot}}) be seen!")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "I () be seen!" (mustache-render-to-string template context)
    "Basic Context Miss Interpolation :: Failed context lookups should default to empty strings."
    template data partials)))

(deftest |test-spec-79| (spec-suite)
 (let* ((template "I ({{{cannot}}}) be seen!")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "I () be seen!" (mustache-render-to-string template context)
    "Triple Mustache Context Miss Interpolation :: Failed context lookups should default to empty strings."
    template data partials)))

(deftest |test-spec-80| (spec-suite)
 (let* ((template "I ({{&cannot}}) be seen!")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "I () be seen!" (mustache-render-to-string template context)
    "Ampersand Context Miss Interpolation :: Failed context lookups should default to empty strings."
    template data partials)))

(deftest |test-spec-81| (spec-suite)
 (let* ((template "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"")
        (data '((:person (:name . "Joe"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Joe\" == \"Joe\""
    (mustache-render-to-string template context)
    "Dotted Names - Basic Interpolation :: Dotted names should be considered a form of shorthand for sections."
    template data partials)))

(deftest |test-spec-82| (spec-suite)
 (let* ((template
         "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\"")
        (data '((:person (:name . "Joe"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Joe\" == \"Joe\""
    (mustache-render-to-string template context)
    "Dotted Names - Triple Mustache Interpolation :: Dotted names should be considered a form of shorthand for sections."
    template data partials)))

(deftest |test-spec-83| (spec-suite)
 (let* ((template
         "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\"")
        (data '((:person (:name . "Joe"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Joe\" == \"Joe\""
    (mustache-render-to-string template context)
    "Dotted Names - Ampersand Interpolation :: Dotted names should be considered a form of shorthand for sections."
    template data partials)))

(deftest |test-spec-84| (spec-suite)
 (let* ((template "\"{{a.b.c.d.e.name}}\" == \"Phil\"")
        (data '((:a (:b (:c (:d (:e (:name . "Phil"))))))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Phil\" == \"Phil\""
    (mustache-render-to-string template context)
    "Dotted Names - Arbitrary Depth :: Dotted names should be functional to any level of nesting."
    template data partials)))

(deftest |test-spec-85| (spec-suite)
 (let* ((template "\"{{a.b.c}}\" == \"\"")
        (data '((:a)))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\" == \"\"" (mustache-render-to-string template context)
    "Dotted Names - Broken Chains :: Any falsey value prior to the last part of the name should yield ''."
    template data partials)))

(deftest |test-spec-86| (spec-suite)
 (let* ((template "\"{{a.b.c.name}}\" == \"\"")
        (data '((:a (:b)) (:c (:name . "Jim"))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"\" == \"\"" (mustache-render-to-string template context)
    "Dotted Names - Broken Chain Resolution :: Each part of a dotted name should resolve only against its parent."
    template data partials)))

(deftest |test-spec-87| (spec-suite)
 (let* ((template "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\"")
        (data
         '((:a (:b (:c (:d (:e (:name . "Phil"))))))
           (:b (:c (:d (:e (:name . "Wrong")))))))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "\"Phil\" == \"Phil\""
    (mustache-render-to-string template context)
    "Dotted Names - Initial Resolution :: The first part of a dotted name should resolve as any other name."
    template data partials)))

(deftest |test-spec-88| (spec-suite)
 (let* ((template "| {{string}} |")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| --- |" (mustache-render-to-string template context)
    "Interpolation - Surrounding Whitespace :: Interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-89| (spec-suite)
 (let* ((template "| {{{string}}} |")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| --- |" (mustache-render-to-string template context)
    "Triple Mustache - Surrounding Whitespace :: Interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-90| (spec-suite)
 (let* ((template "| {{&string}} |")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "| --- |" (mustache-render-to-string template context)
    "Ampersand - Surrounding Whitespace :: Interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-91| (spec-suite)
 (let* ((template
         "  {{string}}
")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  ---
"
    (mustache-render-to-string template context)
    "Interpolation - Standalone :: Standalone interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-92| (spec-suite)
 (let* ((template
         "  {{{string}}}
")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  ---
"
    (mustache-render-to-string template context)
    "Triple Mustache - Standalone :: Standalone interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-93| (spec-suite)
 (let* ((template
         "  {{&string}}
")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  ---
"
    (mustache-render-to-string template context)
    "Ampersand - Standalone :: Standalone interpolation should not alter surrounding whitespace."
    template data partials)))

(deftest |test-spec-94| (spec-suite)
 (let* ((template "|{{ string }}|")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|---|" (mustache-render-to-string template context)
    "Interpolation With Padding :: Superfluous in-tag whitespace should be ignored."
    template data partials)))

(deftest |test-spec-95| (spec-suite)
 (let* ((template "|{{{ string }}}|")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|---|" (mustache-render-to-string template context)
    "Triple Mustache With Padding :: Superfluous in-tag whitespace should be ignored."
    template data partials)))

(deftest |test-spec-96| (spec-suite)
 (let* ((template "|{{& string }}|")
        (data '((:string . "---")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|---|" (mustache-render-to-string template context)
    "Ampersand With Padding :: Superfluous in-tag whitespace should be ignored."
    template data partials)))

(deftest |test-spec-97| (spec-suite)
 (let* ((template "{{=<% %>=}}(<%text%>)")
        (data '((:text . "Hey!")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "(Hey!)" (mustache-render-to-string template context)
    "Pair Behavior :: The equals sign (used on both sides) should permit delimiter changes."
    template data partials)))

(deftest |test-spec-98| (spec-suite)
 (let* ((template "({{=[ ]=}}[text])")
        (data '((:text . "It worked!")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "(It worked!)" (mustache-render-to-string template context)
    "Special Characters :: Characters with special meaning regexen should be valid delimiters."
    template data partials)))

(deftest |test-spec-99| (spec-suite)
 (let* ((template
         "[
{{#section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|#section|
  {{data}}
  |data|
|/section|
]
")
        (data '((:section . t) (:data . "I got interpolated.")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"
    (mustache-render-to-string template context)
    "Sections :: Delimiters set outside sections should persist." template data
    partials)))

(deftest |test-spec-100| (spec-suite)
 (let* ((template
         "[
{{^section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|^section|
  {{data}}
  |data|
|/section|
]
")
        (data '((:section) (:data . "I got interpolated.")))
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"
    (mustache-render-to-string template context)
    "Inverted Sections :: Delimiters set outside inverted sections should persist."
    template data partials)))

(deftest |test-spec-101| (spec-suite)
 (let* ((template
         "[ {{>include}} ]
{{= | | =}}
[ |>include| ]
")
        (data '((:value . "yes")))
        (partials '((:include . ".{{value}}.")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[ .yes. ]
[ .yes. ]
"
    (mustache-render-to-string template context)
    "Partial Inheritence :: Delimiters set in a parent template should not affect a partial."
    template data partials)))

(deftest |test-spec-102| (spec-suite)
 (let* ((template
         "[ {{>include}} ]
[ .{{value}}.  .|value|. ]
")
        (data '((:value . "yes")))
        (partials '((:include . ".{{value}}. {{= | | =}} .|value|.")))
        (context (mustache-context :data data :partials partials)))
   (assert-equal "[ .yes.  .yes. ]
[ .yes.  .|value|. ]
"
    (mustache-render-to-string template context)
    "Post-Partial Behavior :: Delimiters set in a partial should not affect the parent template."
    template data partials)))

(deftest |test-spec-103| (spec-suite)
 (let* ((template "| {{=@ @=}} |")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|  |" (mustache-render-to-string template context)
    "Surrounding Whitespace :: Surrounding whitespace should be left untouched."
    template data partials)))

(deftest |test-spec-104| (spec-suite)
 (let* ((template
         " | {{=@ @=}}
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal " | 
"
    (mustache-render-to-string template context)
    "Outlying Whitespace (Inline) :: Whitespace should be left untouched."
    template data partials)))

(deftest |test-spec-105| (spec-suite)
 (let* ((template
         "Begin.
{{=@ @=}}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Standalone Tag :: Standalone lines should be removed from the template."
    template data partials)))

(deftest |test-spec-106| (spec-suite)
 (let* ((template
         "Begin.
  {{=@ @=}}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Indented Standalone Tag :: Indented standalone lines should be removed from the template."
    template data partials)))

(deftest |test-spec-107| (spec-suite)
 (let* ((template
         "|
{{= @ @ =}}
|")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|
|"
    (mustache-render-to-string template context)
    "Standalone Line Endings :: \"\\r\\n\" should be considered a newline for standalone tags."
    template data partials)))

(deftest |test-spec-108| (spec-suite)
 (let* ((template
         "  {{=@ @=}}
=")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "=" (mustache-render-to-string template context)
    "Standalone Without Previous Line :: Standalone tags should not require a newline to precede them."
    template data partials)))

(deftest |test-spec-109| (spec-suite)
 (let* ((template
         "=
  {{=@ @=}}")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "=
"
    (mustache-render-to-string template context)
    "Standalone Without Newline :: Standalone tags should not require a newline to follow them."
    template data partials)))

(deftest |test-spec-110| (spec-suite)
 (let* ((template "|{{= @   @ =}}|")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "||" (mustache-render-to-string template context)
    "Pair with Padding :: Superfluous in-tag whitespace should be ignored."
    template data partials)))

(deftest |test-spec-111| (spec-suite)
 (let* ((template "12345{{! Comment Block! }}67890")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "1234567890" (mustache-render-to-string template context)
    "Inline :: Comment blocks should be removed from the template." template
    data partials)))

(deftest |test-spec-112| (spec-suite)
 (let* ((template
         "12345{{!
  This is a
  multi-line comment...
}}67890
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "1234567890
"
    (mustache-render-to-string template context)
    "Multiline :: Multiline comments should be permitted." template data
    partials)))

(deftest |test-spec-113| (spec-suite)
 (let* ((template
         "Begin.
{{! Comment Block! }}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Standalone :: All standalone comment lines should be removed." template
    data partials)))

(deftest |test-spec-114| (spec-suite)
 (let* ((template
         "Begin.
  {{! Indented Comment Block! }}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Indented Standalone :: All standalone comment lines should be removed."
    template data partials)))

(deftest |test-spec-115| (spec-suite)
 (let* ((template
         "|
{{! Standalone Comment }}
|")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "|
|"
    (mustache-render-to-string template context)
    "Standalone Line Endings :: \"\\r\\n\" should be considered a newline for standalone tags."
    template data partials)))

(deftest |test-spec-116| (spec-suite)
 (let* ((template
         "  {{! I'm Still Standalone }}
!")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "!" (mustache-render-to-string template context)
    "Standalone Without Previous Line :: Standalone tags should not require a newline to precede them."
    template data partials)))

(deftest |test-spec-117| (spec-suite)
 (let* ((template
         "!
  {{! I'm Still Standalone }}")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "!
"
    (mustache-render-to-string template context)
    "Standalone Without Newline :: Standalone tags should not require a newline to follow them."
    template data partials)))

(deftest |test-spec-118| (spec-suite)
 (let* ((template
         "Begin.
{{!
Something's going on here...
}}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Multiline Standalone :: All standalone comment lines should be removed."
    template data partials)))

(deftest |test-spec-119| (spec-suite)
 (let* ((template
         "Begin.
  {{!
    Something's going on here...
  }}
End.
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "Begin.
End.
"
    (mustache-render-to-string template context)
    "Indented Multiline Standalone :: All standalone comment lines should be removed."
    template data partials)))

(deftest |test-spec-120| (spec-suite)
 (let* ((template
         "  12 {{! 34 }}
")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "  12 
"
    (mustache-render-to-string template context)
    "Indented Inline :: Inline comments should not strip whitespace" template
    data partials)))

(deftest |test-spec-121| (spec-suite)
 (let* ((template "12345 {{! Comment Block! }} 67890")
        (data 'nil)
        (partials 'nil)
        (context (mustache-context :data data :partials partials)))
   (assert-equal "12345  67890" (mustache-render-to-string template context)
    "Surrounding Whitespace :: Comment removal should preserve surrounding whitespace."
    template data partials)))
