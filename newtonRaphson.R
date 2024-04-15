# Definir la función de Newton-Raphson
newtonraphson = function(fun, x0, tol = 0.000000005, maxiter = 100){
  numiter = 0
  fp = Deriv(fun) # fun’ respecto a x
  correccion = -fun(x0)/fp(x0)
  # Una tabla para imprimir la corrida del programa
  tabla = data.frame(A=" ", B= x0, ErrorEstimado=correccion)
  while (abs(correccion) >= tol && numiter <= maxiter) {
    numiter = numiter + 1
    # "correccion" podría accidentalmente convertirse en NA (dato no disponible)
    if (fp(x0) == 0 | is.na(correccion)) stop("Problemas en la corrección")
    x1 = x0 + correccion
    correccion = -fun(x1)/fp(x1)
    x0 = x1
    tabla =rbind(tabla,data.frame(A=" ", B= x0, ErrorEstimado=correccion))
  }
  if (numiter > maxiter){ 
    warning("Se alcanzó el máximo número de iteraciones.")
    cat("Estado:\n")
    cat("k = ", numiter, "x = ", x1, " f(x) = ", fun(x1), "Error estimado <= ", correccion, "\n\n")
  } else {
    colnames(tabla)=c("k", "xk"," -fun(x1)/fp(x1)")
    print(tabla,right=F) #nombres alineados a la derecha
  }
  
  # Devolver el último valor encontrado
  return(x0)
}

# Definir la función
f = function(x) x - cos(x) 

# Aplicar el método de Newton-Raphson
# Aquí es donde se aplican los valores a las funciones

#(Función, Punto inicial,Tolerancia, limite de iteraciones)
raiz <- newtonraphson(f, 0, 0.00000005, 10)




# Graficar la función y las iteraciones
library(ggplot2)

# Generar secuencia de valores de x para la gráfica
x_vals <- seq(-2, 2, length.out = 100)

# Calcular los valores de la función original en la secuencia de x_vals
y_vals <- f(x_vals)

# Crear el dataframe para la gráfica de la función original
df_funcion <- data.frame(x = x_vals, y = y_vals)

# Crear el dataframe para los puntos de iteración
df_iteraciones <- data.frame(x = raiz, y = f(raiz))

# Graficar
ggplot() +
  geom_line(data = df_funcion, aes(x = x, y = y), color = "blue", linewidth = 1) +  # Función original
  geom_point(data = df_iteraciones, aes(x = x, y = y), color = "red", size = 3) +  # Iteraciones de Newton-Raphson
  labs(title = "Método de Newton-Raphson para encontrar la raíz de la función",
       x = "x",
       y = "f(x)")
