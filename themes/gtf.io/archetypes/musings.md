---
title: "{{ replace .Name "-" " " | title }}"
date: {{ .Date }}
lastmod: {{ .Date }}
draft: true

tags:
- add
- tags
- here

citations:
- id: "example-citation"
  src: "https://example.com/citation"
  label: "This is an example citation"
  retrieved: "{{ .Date }}"

---

_Lead paragraph goes here!_

And the rest of the content...
