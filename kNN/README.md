# kNN

kNN относит объект u к тому классу, элементов которого больше среди k ближайших соседей:

![kNN](kNN.png)

### Программная реализация на языке R

```R
kNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}
```

После завершения алгоритма LOO, получаем оптимальное k = 6.

### Карта классификации для 6NN
<br/><br/>

<img src="kNN_kk.png" width="600">

<a href="https://github.com/davilexx/ml1">На главную</a>
