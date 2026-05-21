# Browser Reference Library Assets

The static webR analysis page only supports these small browser libraries:

- `openspecy-medoid.rds`
- `openspecy-model.rds`

Do not add the full Open Specy reference library to this directory. Larger
libraries should stay server-side or use a separate storage/distribution plan.

Generate the supported assets with:

```sh
Rscript tools/build-static-webr-site.R --export-libs
```

The build script refuses unsupported library names and checks asset size before
copying files into `site/data/`.
