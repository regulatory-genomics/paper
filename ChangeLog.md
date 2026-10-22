# Changelog for paper

## Unreleased changes

- Register `sec:`-labelled headings in the cross-reference filter. Section
  references such as `[@sec:methods]` now resolve to the referenced section's
  title, hyperlinked to the heading, instead of rendering an undefined
  reference marker. Duplicate `sec:` labels are build errors.
- Add optional `lead-contact` author field. At most one author may set
  `lead-contact: true`; the author receives a numeric superscript mark (the
  next number after the affiliation marks) and a `ⁿLead contact` note is
  rendered in the institute block — affiliations, equal-contribution note,
  lead contact, and correspondence, left-aligned as one block directly below
  the centered author names — in PDF, HTML, and DOCX output. The
  corresponding-author envelope mark (✉) is always the last author mark.
- Change the equal-contribution author mark from `*` to a shared number
  allocated after the affiliation marks; the note now reads
  `ⁿThese authors contributed equally`.
- Add `citation-key` metadata field: a map of citation key aliases, e.g.
  `citation-key: {id1: doi:10.1038/s41592-023-02139-9}`. Citations written as
  `[@cite:id1]` are replaced by the mapped identifier before citations are
  processed; citing an undefined key is a build error.
