# Paper Format Specification

This document describes the input format used by `paper`. A Paper project is a
directory containing one YAML metadata file and one or more Pandoc Markdown
files. The metadata file defines the document order, while the Markdown files
contain the manuscript content.

Paper uses Pandoc's document model and Pandoc Markdown extensions. It is not a
separate Markdown dialect; standard Pandoc Markdown syntax remains available
unless noted below.

## Project Layout

An initialized project has this general layout:

```text
project/
├── metadata.yaml
├── introduction.md
├── results.md
├── discussion.md
├── methods.md
├── acknowledgements.md
├── supplement.md
├── bibliography.bib       # created or extended when DOI citations are used
└── figures/
    └── ...
```

The section files and `figures/` directory are examples, not reserved names.
Any files listed by `metadata.yaml` may be used.

Create a project with:

```sh
paper init PROJECT_DIR
```

The command fails if `PROJECT_DIR` already exists. The generated project
contains `metadata.yaml` and example section files that can be replaced or
edited.

## Metadata File

The build entry point is `metadata.yaml`. It must contain a `contents` key
whose value is a YAML list of Markdown file names:

```yaml
contents:
  - introduction.md
  - results.md
  - discussion.md
  - methods.md
```

Paths are resolved relative to the directory containing `metadata.yaml`.
Files are read and concatenated in list order. A missing `contents` key or a
non-list value is an error.

Supplementary material is specified independently:

```yaml
supplement:
  - supplement.md
  - supplementary-methods.md
```

Supplementary files are read in list order and processed separately for
citations and cross-references. They are then made available to the output
writers as supplementary blocks. If `supplement` is omitted, the document has
no supplementary material.

### Common Metadata

The following fields are used by Paper or passed through to Pandoc templates:

```yaml
title: "A Paper Title"
short-title: A Paper Title
keywords:
  - Markdown
  - Pandoc
abstract: |
  A paragraph describing the manuscript.
```

`short-title` is also used as the output file stem. For example, a short title
of `A Paper Title` produces files named `A Paper Title.tex`,
`A Paper Title.html`, or `A Paper Title.docx`. If it is absent, the stem is
`paper`.

Metadata not consumed directly by Paper is retained and passed to Pandoc. This
allows templates and Pandoc writers to use fields such as:

```yaml
link-citations: true
watermark: DRAFT
```

The metadata from `metadata.yaml` is merged with metadata from the individual
Markdown files. Project metadata is applied last when the document is read.

Paper also adds or replaces several internal fields during loading:

```yaml
chapters: true
chaptersDepth: 1
chapDelim: []
```

These fields control the cross-reference numbering implementation and should
not normally be changed in section files.

### Authors

Authors are represented as a list of maps. Each author must have a `name`.
The other fields are optional:

```yaml
author:
  - name: Kai Zhang
    email: kai@example.org
    affiliations:
      - Department of Biology, Example University
    corresponding: true
    equal_contribution: false
    marks:
      - 1
```

Supported author fields:

| Field | Type | Meaning |
| --- | --- | --- |
| `name` | string | Display name. Required in practice. |
| `email` | string | Email address used for correspondence output. |
| `affiliations` | list of strings | Affiliations associated with the author. |
| `corresponding` | boolean | Marks the author as corresponding author. |
| `equal_contribution` | boolean | Retained in author metadata for templates. |
| `marks` | list of strings | Additional author marks. |

Paper deduplicates affiliations and assigns numeric superscript marks in the
order in which unique affiliations first occur. For LaTeX, authors and
affiliations are supplied as separate Pandoc metadata fields. For HTML and
DOCX, Paper inserts formatted author, affiliation, and correspondence blocks
into the document.

If `corresponding` is true, `email` should be provided. The HTML and DOCX
author formatter assumes that corresponding authors have an email address.

## Markdown Input

Each file listed in `contents` or `supplement` is parsed as Pandoc Markdown
with the following notable extensions enabled:

- YAML metadata blocks
- Pandoc title blocks
- ATX headings and header attributes
- Footnotes and inline notes
- Pipe, simple, multiline, and grid tables
- Table captions
- Implicit figures
- Citations
- Fenced code blocks and code attributes
- Raw LaTeX and raw HTML
- LaTeX math and macros
- Native and fenced divs and spans
- Link attributes and implicit header references
- Definition lists, example lists, fancy lists, and task lists
- Strikeout, superscript, and subscript
- Smart typography and escaped line breaks

Pandoc automatic identifiers are not enabled. Add explicit identifiers when a
heading, figure, or table must be referenced.

An unnumbered heading can be written with the `{-}` attribute:

```markdown
# Introduction {-}
```

## Figures

Use standard Pandoc image syntax and give figures an explicit `fig:` label:

```markdown
![A figure caption](figures/overview.png){#fig:overview}
```

The label must begin with `fig:`. Paper recognizes it as a figure, assigns a
number, prefixes the caption with `Fig. N |`, and records the identifier for
cross-references.

Captions may contain multiple paragraphs. The automatic caption prefix is
inserted into the first paragraph or plain block. Other caption block types are
not supported by the prefixing filter.

Subfigures can be represented with caption text containing emphasized labels,
for example:

```markdown
![Results.
**a,** First condition.
**b,** Second condition.](figures/results.png){#fig:results}
```

When `--no-embed-fig` is not supplied, image paths are converted to absolute
paths before writing. With `--no-embed-fig`, figures are moved to generated
`Figures` and `Supplementary Figures` sections at the end of the document.

### Supplementary Figures

Put supplementary figures in a Markdown file listed under `supplement` in
`metadata.yaml`:

```yaml
contents:
  - introduction.md
  - results.md

supplement:
  - supplement.md
```

Use the `supp_fig:` label prefix for supplementary figures:

```markdown
![Supplementary measurements](figures/supplementary-measurements.png){#supp_fig:measurements}
```

Reference the figure with the same label:

```markdown
The supplementary measurements are shown in [@supp_fig:measurements].
```

Paper numbers supplementary figures separately from main figures and formats
their captions and references as `Supplementary Fig. N`. Use `supp_fig:` rather
than `fig:` when the figure belongs to supplementary material:

```markdown
![Main figure](figures/main.png){#fig:main}
![Supplementary figure](figures/supplementary.png){#supp_fig:supplementary}
```

With `--no-embed-fig`, supplementary figures are collected into the generated
`Supplementary Figures` section at the end of the document. Without that flag,
they remain in their original positions in the supplementary content.

## Tables

Tables use Pandoc table syntax. A table label should begin with `tbl:`. For
example:

```markdown
Table: Summary of measurements {#tbl:summary}

| Sample | Value |
|:-------|------:|
| A      | 12    |
| B      | 18    |
```

The label is used for numbering and cross-references. Paper also recognizes a
table label embedded in the first caption line and can wrap the table in an
internal labeled container for processing.

Table captions are prefixed with `Table N |`. The caption's first paragraph or
plain block must be suitable for inline prefix insertion.

## Sections and Cross-References

Cross-reference labels use a type prefix followed by a colon and an identifier:

| Label prefix | Meaning | Caption/reference prefix |
| --- | --- | --- |
| `fig:` | Figure | `Fig.` or `figure` |
| `tbl:` | Table | `Table` or `table` |
| `sec:` | Section | `Section` or `section` |
| `supp_fig:` | Supplementary figure | `Supplementary Fig.` |
| `ext_fig:` | Extended Data figure | `Extended Data Fig.` |
| custom prefix | Custom reference type | The prefix text |

Reference a labeled object with a Pandoc citation:

```markdown
See [@fig:overview] and [@tbl:summary].
```

Section references require an explicit section identifier:

```markdown
# Methods {#sec:methods}

The procedure is described in [@sec:methods].
```

The case of a reference identifier is significant for lookup after its type
prefix has been recognized. A label such as `fig:Overview` should therefore be
referenced consistently.

Multiple references can be grouped:

```markdown
See [@fig:overview; @fig:results].
```

Adjacent references of the same type can be formatted as a range when their
assigned numbers are consecutive:

```markdown
See [@fig:first; @fig:second; @fig:third].
```

Pandoc citation prefixes and suffixes are preserved. For example:

```markdown
See [see @fig:overview, page 4].
```

An unresolved cross-reference is rendered as a visible `¿label?` marker and a
diagnostic is emitted. Duplicate labels are errors.

## Citations and Bibliographies

Paper uses Pandoc citation syntax. DOI references have the special form
`doi:`:

```markdown
The method follows [@doi:10.1038/s41592-023-02139-9].
```

For DOI references, Paper:

1. Finds the citation identifiers in the main and supplementary content.
2. Looks up missing DOI entries at `https://doi.org/` using the BibTeX media type.
3. Adds the returned entries to the bibliography cache.
4. Runs Pandoc citeproc and inserts the bibliography into the document.

The default cache file is `bibliography.bib` in the project directory. A
pre-existing BibTeX file is read first, and only missing DOI identifiers are
requested. Use `--bib-cache FILE` to select another cache file.

Use `--disable-cache` to disable the cache. In that mode Paper fetches the
required DOI references for the current build instead of reading or updating a
cache file.

The default citation style is the bundled Nature CSL style. Citation metadata
such as `link-citations` can be supplied in `metadata.yaml` and is passed to
Pandoc.

## Build Commands

Build a project with:

```sh
paper build PROJECT_DIR
```

The command always writes a LaTeX file. Additional outputs are selected with
flags:

| Flag | Output |
| --- | --- |
| `--output-raw` | JSON representation of the processed Pandoc AST (`.raw`). |
| `--output-pdf` | PDF generated by running `tectonic` on the generated LaTeX. |
| `--output-html` | HTML output. |
| `--output-docx` | DOCX output. |
| `--self-contained` | Embed resources in HTML using Pandoc's self-contained processing. |
| `--no-embed-fig` | Do not embed or absolutize figure resources; place figures at the end. |
| `--disable-cache` | Do not use the bibliography cache. |
| `--bib-cache FILE` or `-b FILE` | Use `FILE` as the bibliography cache. |
| `--out-dir DIR` or `-o DIR` | Write generated files to `DIR`. |

The positional `PROJECT_DIR` is both the project directory and the initial
working directory for the build. Unless `--out-dir` is supplied, generated
files are written to the parent directory of `PROJECT_DIR`.

Custom templates can be selected with:

```sh
paper build PROJECT_DIR \
  --latex-template journal.tex \
  --html-template journal.html \
  --docx-template journal.docx
```

The DOCX template is used as the reference document. The LaTeX and HTML files
are used as Pandoc writer templates.

## Complete Example

`metadata.yaml`:

```yaml
title: "A Small Study"
short-title: small-study
author:
  - name: Ada Researcher
    email: ada@example.org
    affiliations:
      - Example Institute
    corresponding: true
contents:
  - introduction.md
  - results.md
supplement:
  - supplement.md
link-citations: true
```

`results.md`:

```markdown
# Results

We observed the effect shown in [@fig:effect]. The measurements are listed in
[@tbl:measurements].

![Effect of the treatment](figures/effect.png){#fig:effect}

Table: Measurements {#tbl:measurements}

| Condition | Mean |
|:----------|-----:|
| Control   | 1.2  |
| Treatment | 2.7  |

The analysis follows [@doi:10.1038/s41592-023-02139-9].
```

Build all requested formats with:

```sh
paper build small-study --output-pdf --output-html --output-docx
```

## Limitations and Failure Modes

- `metadata.yaml` must contain `contents` as a list of file names.
- `supplement`, when present, must also be a list of file names.
- Author maps should include `name`; missing required fields can cause a build
  failure.
- Corresponding authors should include `email` because the non-LaTeX author
  formatter uses it when generating correspondence text.
- Figure and table caption prefixing expects the first caption block to be a
  paragraph or plain block.
- Duplicate reference labels are errors.
- Undefined cross-references are not silently removed; they appear as visible
  markers.
- DOI lookup requires network access to `doi.org`.
- PDF generation requires the `tectonic` executable on `PATH`.
- Automatic heading identifiers are disabled; use explicit `{#id}` attributes.
- Custom reference prefixes are recognized by the cross-reference filter, but
  there is currently no user-facing metadata format for defining custom
  formatter behavior.
