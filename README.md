# 1NN

<b>Алгоритм ближайшего соседа (1NN)</b> – относит классифицируемый объект u ∈ X к тому классу, которому принадлежит его ближайший сосед:

![1NN](1nn.png)

Преимущество
- Простота реализации.

Недостатки
- Неустойчивость к погрешностям - выбросам.
- Отсутствие параметров, которые можно было бы настраивать по вы-борке. Алгоритм полностью зависит от того, насколько удачно выбранаметрика ρ.
- Низкое качество классификации.

# kNN

<b>kNN (k Nearest Neighbor или k Ближайших Соседей)</b> — это один из самых простых алгоритмов классификации, также иногда используемый в задачах регрессии. Благодаря своей простоте, он является хорошим примером, с которого можно начать знакомство с областью Machine Learning.

<b>Алгоритм</b>

Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:

<ul>
  <li>Вычислить расстояние до каждого из объектов обучающей выборки</li>
  <li>Отобрать k объектов обучающей выборки, расстояние до которых минимально</li>
  <li>Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей</li>
</ul>

 <img src="https://cdn.analyticsvidhya.com/wp-content/uploads/2018/03/knn3.png" width="550" height="500"> 
 
<p>Пример классификации k-ближайших соседей. Тестовый образец (зелёный круг) должен быть классифицирован как синий квадрат (класс 1) или как красный треугольник (класс 2). Если k = 3, то он классифицируется как 2-й класс, потому что внутри меньшего круга 2 треугольника и только 1 квадрат. Если k = 5, то он будет классифицирован как 1-й класс (3 квадрата против 2 треугольников внутри большего круга)</p>

kNN относит объект u к тому классу, элементов которого больше среди k ближайших соседей:

![kNN](kNN.png)

