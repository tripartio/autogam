# autogam 0.1.0

* Use bam(discrete=TRUE, method='fREML') by default.
* Update required R version to >= 4.2.0 because it uses the |> pipe with placeholder.
* Create `autogam` S3 object with support for all `mgcv::gam` s3 methods. The `autogam` methods mostly call the `mgcv::gam` methods. For now, only the `print()` method is lightly modified to add performance measures.

# autogam 0.0.1

* Initial CRAN submission.
