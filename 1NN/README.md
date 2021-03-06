# 1NN

1NN (алгоритм ближайшего соседа) – относит классифицируемый объект u ∈ X к тому классу, которому принадлежит его ближайший сосед:

![1NN](1nn.png)

### Программная реализация на языке R

```R
## применяем метод
NN <- function(xl, z) {
  ## сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  ## получаем классы первых соседей
  classes <- orderedXl[1, n + 1]
  return (classes)
}
```

### Работа 1NN на выборке ирисов Фишера

<img src="1NN_iris.png" width=600>

### Карта классификации для 1NN
<br/><br/>

<img src="1nn_kk.png" width=600>

Преимущество:
<ul>
<li>простота реализации</li>
</ul>

Недостатки:
<ul>
<li>неустойчивость к погрешностям — выбросам</li>
<li>отсутствие параметров, которые можно было бы настраивать по выборке. Алгоритм полностью зависит от того, насколько удачно выбранаметрика ρ</li>
<li>низкое качество классификации</li>
</ul>

<a href="https://github.com/davilexx/ml1">На главную</a>
