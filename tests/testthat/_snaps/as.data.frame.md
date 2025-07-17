# as.data.frame

    Lotri Estimates (get with `lotriEst()`):
      name lower est upper   fix   label backTransform
    1    h     0   1   Inf FALSE b label         expit
    2    i     0   1     2 FALSE    <NA>          <NA>
    3    j  -Inf   1   Inf  TRUE    <NA>          <NA>
    4    k     0   1     2  TRUE    <NA>          <NA>
    5    l     0   1     2  TRUE    <NA>          <NA>
    
    Matrix:
        m   n
    m 1.0 0.5
    n 0.5 1.0
    
    This matrix has diagonal labels:
    [1] NA  "n"
    

---

     'lotriFix' num [1:2, 1:2] 1 0.5 0.5 1
     - attr(*, "dimnames")=List of 2
      ..$ : chr [1:2] "m" "n"
      ..$ : chr [1:2] "m" "n"
     - attr(*, "lotriLabels")= chr [1:2] NA "n"
     - attr(*, "lotriEst")='data.frame':	5 obs. of  7 variables:
      ..$ name         : chr [1:5] "h" "i" "j" "k" ...
      ..$ lower        : num [1:5] 0 0 -Inf 0 0
      ..$ est          : num [1:5] 1 1 1 1 1
      ..$ upper        : num [1:5] Inf 2 Inf 2 2
      ..$ fix          : logi [1:5] FALSE FALSE TRUE TRUE TRUE
      ..$ label        : chr [1:5] "b label" NA NA NA ...
      ..$ backTransform: chr [1:5] "expit" NA NA NA ...

---

    Lotri Estimates (get with `lotriEst()`):
      name lower est upper   fix   label backTransform
    1    a     0   1   Inf FALSE a label           exp
    2    b     0   1     2 FALSE    <NA>          <NA>
    3    c  -Inf   1   Inf  TRUE    <NA>          <NA>
    4    d     0   1     2  TRUE    <NA>          <NA>
    5    e     0   1     2  TRUE    <NA>          <NA>
    
    Matrix:
        f   g
    f 1.0 0.5
    g 0.5 1.0
    this matrix has fixed elements

---

    Lotri Estimates (get with `lotriEst()`):
       name lower est upper   fix   label backTransform
    1     a     0   1   Inf FALSE a label           exp
    2     b     0   1     2 FALSE    <NA>          <NA>
    3     c  -Inf   1   Inf  TRUE    <NA>          <NA>
    4     d     0   1     2  TRUE    <NA>          <NA>
    5     e     0   1     2  TRUE    <NA>          <NA>
    6     h     0   1   Inf FALSE b label         expit
    7     i     0   1     2 FALSE    <NA>          <NA>
    8     j  -Inf   1   Inf  TRUE    <NA>          <NA>
    9     k     0   1     2  TRUE    <NA>          <NA>
    10    l     0   1     2  TRUE    <NA>          <NA>
    
    $id
        m   n
    m 1.0 0.5
    n 0.5 1.0
    
    $occ
        f   g
    f 1.0 0.5
    g 0.5 1.0
    this matrix has fixed elements
    

