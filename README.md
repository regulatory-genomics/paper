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

### Prebuilt Releases

Tagged releases publish prebuilt binaries for Linux x86_64, macOS x86_64, and
macOS Apple Silicon on the [GitHub Releases](https://github.com/regulatory-genomics/paper/releases)
page. Download the archive for your platform and verify it with the matching
`.sha256` file before extracting it:

```sh
shasum -a 256 -c paper-linux-x86_64.tar.gz.sha256
tar -xzf paper-linux-x86_64.tar.gz
```

The archive contains the `paper` executable. PDF generation additionally
requires [tectonic](https://tectonic-typesetting.github.io/en-US/) to be
installed and available on `PATH`.

### Dependencies:

- [tectonic](https://tectonic-typesetting.github.io/en-US/). 

## Usage

1. Create a new project: `paper init my_project`.

2. Create a PDF output: `paper build my_project --output-pdf`
