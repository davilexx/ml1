# kNN

kNN относит объект u к тому классу, элементов которого больше среди k ближайших соседей:

![kNN](kNN.png)

<h2> Программная реализация на языке R </h2>

В качестве функции расстояния будем использовать обычное расстояние

```R
euclideanDistance <-function(u, v) 
{
  sqrt(sum((u - v) ^ 2))
}
```

Сортируем объекты согласно расстояния до объекта z, создав матрицу расстояний

```R
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <-dim(xl)[1]
  n <-dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l) 
  {
    distances[i, ] <- c(i,metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}
```

Применяем метод kNN: сортируем выборку согласно классифицируемого объекта, получаем классы первых k соседей, составляем таблицу встречаемости каждого класса и находим класс, который доминирует среди первых k соседей

```R
kNN <- function(xl, z, k)
{
  
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  
  classes <- orderedXl[1:k, n + 1]
  
  counts <- table(classes)
  
  class <- names(which.max(counts))
  
  return (class)
}
```

Рисуем выборку

```R
colors <-c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
```

Классификация одного заданного объекта

```R
z <- c(2.7, 1)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
```

Результатом кода будет это:

![result](result.png)

На изображении выше показана классификация выбранного объекта. Он обозначен квадратом. В качестве координат была взята точка (2.7, 1).
