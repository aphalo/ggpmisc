# methylclock

<details>

* Version: 1.6.0
* GitHub: https://github.com/isglobal-brge/methylclock
* Source code: https://github.com/cran/methylclock
* Date/Publication: 2023-04-25
* Number of recursive dependencies: 279

Run `revdepcheck::revdep_details(, "methylclock")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'planet'
      All declared Imports should be used.
    Unexported object imported by a ':::' call: 'minfi:::projectCellType'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    DNAmAge: no visible binding for global variable 'coefHorvath'
    DNAmAge: no visible binding for global variable 'coefHannum'
    DNAmAge: no visible binding for global variable 'coefLevine'
    DNAmAge: no visible binding for global variable 'coefSkin'
    DNAmAge: no visible binding for global variable 'coefPedBE'
    DNAmAge: no visible binding for global variable 'coefWu'
    DNAmAge: no visible binding for global variable 'coefTL'
    DNAmAge: no visible binding for global variable 'coefBLUP'
    DNAmAge: no visible binding for global variable 'coefEN'
    DNAmGA: no visible binding for global variable 'coefKnightGA'
    ...
    plotDNAmAge: no visible binding for global variable '..rr.label..'
    Undefined global functions or variables:
      ..eq.label.. ..p.label.. ..rr.label.. MethylationData age clock
      coefBLUP coefBohlin coefEN coefEPIC coefHannum coefHorvath
      coefKnightGA coefLeeGA coefLevine coefMayneGA coefPedBE coefSkin
      coefTL coefWu cpgs.in data install.packages
      meffil.estimate.cell.counts.from.betas method plCellCpGsThird
    Consider adding
      importFrom("utils", "data", "install.packages")
    to your NAMESPACE file.
    ```

