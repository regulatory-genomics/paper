# Markdown Syntax Showcase

This file demonstrates the Pandoc Markdown features enabled by Paper. The
examples are intentionally small so they can be copied into manuscript files.

## Text Formatting {#syntax-text}

Text can be *emphasized*, **made strong**, or ~~struck out~~. Chemical and
mathematical notation can use subscript H~2~O and superscript x^2^.

Inline code looks like `paper build project`. Code can have attributes such as
`paper`{.command}.

Smart punctuation converts straight quotes to “typographic quotes”, three dots
to an ellipsis…, and two hyphens to an en dash -- or an em dash ---.

This line ends with a hard line break.  
The next line starts immediately below it.

An inline note can be written here^[This is an inline note.]. A regular
footnote can be referenced here.[^example-footnote]

[^example-footnote]: Footnotes can contain ordinary Markdown formatting.

## Links and Spans

Visit the [Pandoc website](https://pandoc.org/ "Pandoc documentation"). A
reference-style link can use [the project homepage][paper-home].

[paper-home]: https://github.com/regulatory-genomics/paper

This is a [bracketed span]{.highlight #important-span}. Spans can carry
classes and identifiers with [inline attributes]{.notice #notice-span}.

## Mathematics

Inline mathematics uses LaTeX syntax: $E = mc^2$.

Display mathematics can be written as:

$$
\int_0^1 x^2\,dx = \frac{1}{3}
$$

The default LaTeX template defines `argmax` and `argmin` as mathematical
operators:

$$
\hat{x} = \argmax_{x \in X} f(x), \qquad
\hat{x} = \argmin_{x \in X} f(x)
$$

LaTeX macros can be declared and used in math:

\newcommand{\R}{\mathbb{R}}

The real numbers are written as $x \in \R$.

## Lists

Unordered lists are useful for short collections:

- First item
- Second item
  - Nested item
  - Another nested item

Ordered lists can start at a custom number:

3. Third step
4. Fourth step

Definition lists associate terms with descriptions:

Pandoc
:   A document converter.

Paper
:   A scientific writing application built on Pandoc.

Example lists preserve numbered examples:

(@example-one) First example.

(@example-two) Second example.

Task lists can track work:

- [x] Draft the introduction
- [ ] Review the results

## Quotes and Blocks

> This is a block quote.
>
> It can contain multiple paragraphs and other Markdown blocks.

Fenced code blocks can specify a language and attributes:

```text
main :: IO ()
main = putStrLn "Hello, Paper"
```

Native divs use three colons and an attribute list:

::: {.important #important-block}
This is a native div with a class and identifier.
:::

Fenced divs use a longer fence:

:::: {.example}
This block can contain another fenced block.

```text
Nested content
```
::::

Line blocks preserve line-oriented text:

| First line
| Second line
| Third line

---

The horizontal rule above separates blocks.

## Comments

Use HTML comment syntax for editorial notes that should remain in the Markdown
source but not be displayed in rendered output:

<!-- This single-line comment is omitted from the rendered document. -->

Comments can span multiple lines:

<!--
This is a multiline editorial comment.
It is hidden from the rendered document.
-->

Visible text before and after a comment is rendered normally.

<!-- TODO: Replace this example sentence before submission. -->

This sentence appears after the hidden TODO comment.

HTML comments can remain in generated HTML source, but web browsers do not
display them. They are omitted when Paper writes LaTeX, PDF, or DOCX content.

Raw HTML is preserved for HTML-capable writers:

<span class="raw-example">Raw HTML content</span>

Raw LaTeX is preserved for LaTeX-capable writers:

\\textit{Raw LaTeX content}

## Tables

### Pipe Table

Table: A pipe table {#tbl:pipe}

| Name | Count | Mean |
|:-----|------:|:----:|
| A    | 12    | 3.2  |
| B    | 18    | 4.1  |

### Simple Table

Table: A simple table {#tbl:simple}

    Name       Count
    ---------  -----
    Control       12
    Treatment     18

### Multiline Table

Table: A multiline table {#tbl:multiline}

-------------------------------------------------------------
 Group           Description                    Result
 -------------  -----------------------------  --------
 Control         Baseline measurement            12
 Treatment       Measurement after treatment     18
-------------------------------------------------------------

### Grid Table

Table: A grid table {#tbl:grid}

+-----------+-------+
| Group     | Count |
+===========+=======+
| Control   | 12    |
+-----------+-------+
| Treatment | 18    |
+-----------+-------+

Tables can be referenced with citations such as [@tbl:pipe].

## Figures

Figures use an explicit `fig:` label and can be referenced with
`[@fig:showcase]`:

![A figure with an explicit caption](figures/Gull.jpg){#fig:showcase width=50%}

Subfigure-style captions can label several panels:

![Panel comparison.
**a,** First panel.
**b,** Second panel.](figures/Gull.jpg){#fig:panels}

Extended Data figures use the `ext_fig:` label prefix:

![An Extended Data figure](figures/Gull.jpg){#ext_fig:example}

See [@fig:showcase], [@fig:panels], and [@ext_fig:example].

Supplementary figures belong in a file listed under `supplement` and use the
`supp_fig:` prefix. See `supplement.md` for a complete example.

## Sections and References

This heading has an explicit identifier and can be referenced as
`[@sec:syntax-text]`. The bundled LaTeX template leaves top-level headings
unnumbered.

Multiple references can be grouped: [@fig:showcase; @fig:panels]. Citation
prefixes and suffixes are also supported: [see @fig:showcase, page 1].

## Citations

DOI citations use the form [@doi:10.1038/s41592-023-02139-9]. Multiple
citations can be written together: [@doi:10.1038/s41592-023-02139-9;
@doi:10.1038/s41586-020-2649-2].

The DOI citations in this example are the same form used in ordinary
manuscript text. Paper downloads missing entries to `bibliography.bib` and
processes citations with the bundled CSL style.

## Metadata Block

A Markdown file can also begin with a YAML metadata block:

```text
---
example-field: example value
example-list:
  - one
  - two
---
```

Project-level metadata in `metadata.yaml` remains the main place for title,
authors, contents, supplementary files, and output settings.
