# Paper: Writing scholarly document using Pandoc markdown

A Pandoc-based app for scientific writing using Markdown.

See [data/template/AGENTS.md](data/template/AGENTS.md) for the complete project format specification,
including metadata, Markdown, figures, tables, citations, cross-references,
and build outputs.

## Installation

```
cabal update
cabal install
```

### Dependencies:

- [tectonic](https://tectonic-typesetting.github.io/en-US/). 

## Usage

1. Create a new project: `paper init my_project`.

2. Create a PDF output: `paper build my_project --output-pdf`
