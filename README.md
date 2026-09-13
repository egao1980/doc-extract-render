# doc-extract-render

Markdown and HTML render/parse for [`doc-extract-protocol`](https://github.com/egao1980/doc-extract-protocol) `extracted-document` trees (Demiurge C3e). Own repo — canned publish forces `provides=(system)`, so a slash system on the protocol never lands on GHCR.

| System | Role |
|--------|------|
| `doc-extract-render` (`stack-doc-extract-render`) | `:markdown` / `:html` |
| [`doc-extract-render-pdf`](https://github.com/egao1980/doc-extract-render-pdf) | `:pdf` via `browser-protocol:print-pdf` |

```lisp
(asdf:load-system "doc-extract-render")
(asdf:load-system "html-backend-plump")   ; needed to parse HTML

(let* ((doc (stack-doc-extract:extract-document
             nil "<html><h1>Hi</h1><p>there</p></html>" :format :html))
       (md (stack-doc-extract-render:render-document doc :markdown))
       (html (stack-doc-extract-render:render-document doc :html :profile :archival)))
  (stack-doc-extract-render:parse-document html :html))
```

| GF | Formats |
|----|---------|
| `render-document` | `:markdown` = `dump-markup`; `:html` = semantic HTML5 + `data-demiurge-*` |
| `parse-document` | `:markdown` = `parse-markup`; `:html` walks `html-protocol` (restore `data-*`, else infer) |
| `strip-render-attributes` | drop comments / `data-demiurge-*` so a re-parse can only hold `:structure` |

`:llm` keeps ids and type; `:archival` also keeps provenance, attrs, annotations. HTML uses `<section>`, headings by depth, tables with `rowspan`/`colspan`, `<img src="hash:…">`, and annotation spans as `<em>` / `<a>`.

HTML parse requires `html-backend-plump` (`*html-backend*`). Tests pull it via `:ci :with` the same way `html-protocol` does.

Conditions: `render-error`, `unknown-render-format` (restart `use-value`).

## License

MIT — see [LICENSE](LICENSE).
