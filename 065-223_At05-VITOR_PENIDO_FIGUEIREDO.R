
#Atribuir valores as medidas pradoes

x = c(100.23, 100.19, 102.02, 99.59, 99.81, 99.86, 99.60, 100.35, 99.38, 100.83,
      99.73, 98.00,  99.72, 101.34,  98.77,  99.94,  99.47,  99.95, 100.33,  99.57,
      100.82, 100.23, 100.68, 101.64, 100.86,  99.28, 101.41, 100.21, 101.85, 101.43, 
      103.00)

x
cusum.plot = function(u0, n, k, d, s){
  
  #Variaveis do grafico 
  LSC = k*s/sqrt(n)
  LIC = -LSC
  S_pos <- c(NULL)
  S_neg <- c(NULL)
  
  #Montando a funcao do grafico
  for (i in 1:length(x)) {
    if(i == 1){
      S_pos[i] <- max(0, x[i] - (u0 + d))
      S_neg[i] <- max(0, (u0 - d) - x[i])
    }else{
      S_pos[i] <- max(0, x[i] - (u0 + d) + S_pos[i-1])
      S_neg[i] <- max(0, (u0 - d) - x[i] + S_neg[i-1])
    }
  }
  
  #Construindo o grafico
  plot(S_pos, type = "n", pch = 19, lwd = 2, col = "black",cex = 1.75,
       
       ylim = c(min( LIC, min(S_pos), min(-S_neg) ) - 1, 
                max(LSC, max(S_pos), max(-S_neg)) +1 ),
       main = "Gráfico de CUSUM de Xi",
       xlab = "AMOSTRA",
       ylab = "SOMA ACUMULADA")
  
  # Colocar os pontos do grafico 
  abline(h = 0, col = "green")
  lines(S_pos, lwd = 2)
  lines(-S_neg, lwd = 2)
  points(S_pos, pch = 19, lwd = 0.75, cex = 0.75)
  points(-S_neg, pch = 18, lwd = 2, cex = 2)
  
  # Acrescentar os limites
  abline(h = LSC, col = "red")
  abline(h = LIC, col = "red")
  
  # Sinalizar os pontos fora dos limites do grafico
  acima <- S_pos > LSC
  abaixo <- S_pos < LIC
  fora <- which(acima | abaixo)
  points(x = fora, y = S_pos[fora], cex = 2, pch = 15, col = "red")
  
  text(x = fora + 0.5, y = LSC - 0.25, "LSC" ,col = "red")  
  text(x = fora + 0.5, y = LIC + 0.25, "LIC" ,col = "red")
}


cusum.plot(100,1,5,0.5,1)
