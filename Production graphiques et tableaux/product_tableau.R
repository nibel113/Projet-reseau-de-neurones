##Production des tableau de résultats à l'aide de kableExtra qui formatte
## directement en format latex
library(kableExtra)

##Shallow 
tab <- data.frame(tab_shal_neur[,c(6,2,4,12)])
rownames(tab) <- NULL


kable(tab,format="latex",booktabs=T,escape=F,col.names = c("Nombre de neurones","$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $","$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $","Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')

##Shallow 32

tab <- data.frame(tab_shal_32[,c(2,4,5,7,8,12)])[1:6,]
rownames(tab) <- NULL
kable(tab,format="latex",booktabs=T,escape=F,col.names = c("$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $","$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $","$d$","$\\ell_1$","$\\ell_2$","Epochs"))%>%
  kable_styling(position = 'center')

##Stat wald glm1

tab <- data.frame("Variable"=names(statWald),"seuil observé"= unname(statWald))

kable(tab,format="latex",booktabs=T,escape=F)%>%
  kable_styling(position = 'center')

## Test ratio de vraisemblance

tab <- drop[-1,]
rownames(tab)[7] <- "$\\log\\{Density\\}$"
kable(tab,format="latex",booktabs=T,escape=F)%>%
  kable_styling(position = 'center')

##Deep 2

tab <- data.frame(tab_deep2[1:6,c(2,4,7,8,16)])
tab$structure <-paste(tab$flag_hidden1,tab$flag_hidden2,sep = ",")
tab <- tab[c(1,3,2,4,5,6),c(6,1,2,5)]
rownames(tab) <- NULL

kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')


##Deep 2 tuning hyperparamètres
colnames(tab_deep2_88)[c(9,10)] <- colnames(tab_deep2_32_16)[c(9,10)]
tab <- rbind(tab_deep2_88[,c(2,4,5,6,7,8,9,10,16)],tab_deep2_32_16[,c(2,4,5,6,7,8,9,10,14)])

ind <- order(tab$metric_val_loss)
tab <- tab[ind,]

tab$structure <-paste("(",tab$flag_hidden1,",",tab$flag_hidden2,")",sep = "")
tab <- tab[c(2,1,3,4,5,6),c(10,1,2,3,4,7,8,9)]
rownames(tab) <- NULL

kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "$d_1$",
                    "$d_2$",
                    "$\\ell_1$",
                    "$\\ell_2$",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')


##Deep 3

tab <- tab_deep3[1:6,c(2,4,7,8,9,15)]
tab$structure <-paste("(",tab$flag_hidden1,",",tab$flag_hidden2,",",tab$flag_hidden3,")",sep = "")
tab <- tab[,c(7,1,2,6)]
rownames(tab) <- NULL

kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')


tab <- rbind(tab_deep3_8_8_8[,c(2,4,5,6,7,8,9,10,11,15)],tab_deep3_32_16_8[-1,c(2,4,5,6,7,8,9,10,11,15)])
ind <- order(tab$metric_val_loss)
tab <- tab[ind,]
tab <- tab[1:6,]

tab$structure <-paste("(",tab$flag_hidden1,",",tab$flag_hidden2,",",tab$flag_hidden3,")",sep = "")

tab <- tab[,c(11,1,2,3,4,8,9,10)]
rownames(tab) <- NULL


kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "$d_1$",
                    "$d_2$",
                    "$\\ell_1$",
                    "$\\ell_2$",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')


##deep 4

tab <- tab_deep4[1:6,c(2,4,8,9,10,11,17)]
tab$structure <-paste("(",tab$flag_hidden1,",",tab$flag_hidden2,",",tab$flag_hidden3,",",tab$flag_hidden4,")",sep = "")
tab <- tab[,c(8,1,2,7)]
rownames(tab) <- NULL

kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')

tab <- rbind(tab_deep4_16[,c(2,4,5,6,7,8,9,10,11,12,13,18)],tab_deep4_16_32_8_16[,c(2,4,5,6,7,8,9,10,11,12,13,17)])
ind <- order(tab$metric_val_loss)
tab <- tab[ind,]
tab <- tab[1:6,]

tab$structure <-paste("(",tab$flag_hidden1,",",tab$flag_hidden2,",",tab$flag_hidden3,",",tab$flag_hidden4,")",sep = "")

tab <- tab[,c(13,1,2,3,4,5,10,11,12)]
rownames(tab) <- NULL


kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Nombre de neurones",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "$d_1$",
                    "$d_2$",
                    "$d_3$",
                    "$\\ell_1$",
                    "$\\ell_2$",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')


##results comparaison

## shallow
min(hist_shallow$metrics$val_loss)
length(hist_shallow$metrics$val_loss)
min(hist_shallow$metrics$loss)

val_loss <- numeric()
loss <- numeric()
epochs <- numeric()


vector <- list(hist_shallow,history_deep2,hist_deep3,hist_deep4,
               hist_shallow_embed,hist_deep2_embeded,history_deep3_embeded,hist_deep4_embeded,
               hist_cann_shallow,hist_cann_deep2,hist_cann_deep3,hist_cann_deep4)

for(i in 1:12){
  val_loss <- c(val_loss,min(vector[[i]]$metrics$val_loss))
  loss <- c(loss,min(vector[[i]]$metrics$loss))
  epochs <- c(epochs,length(vector[[i]]$metrics$val_loss))
  
}


eval_loss <- c(score_shallow,score_deep2,score_deep3,score_deep4,
               score_shallow_embeded,score_deep2_embed,score_deep3_embed,score_deep4_embeded,
               score_cann_shallow,score_cann_deep2,score_cann_deep3,score_cann_deep4)
modele <- c("Shallow","Deep2","Deep3","Deep4",
            "Shallow_embedding","Deep2_embedding","Deep3_embedding","Deep4_embedding",
            "Cann_shallow","Cann_deep2","Cann_deep3","Cann_deep4")

tab <- data.frame(modele,eval_loss,loss,val_loss,epochs)
ind <- order(tab$eval_loss)
tab <- tab[ind,]
rownames(tab) <- NULL

kable(tab,
      format="latex",
      booktabs=T,
      escape=F,
      col.names = c("Modèle",
                    "$\\mathcal{L}\\left\\{\\mathcal{T},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{D},\\hat{\\lambda} \\right\\} $",
                    "$\\mathcal{L}\\left\\{\\mathcal{V},\\hat{\\lambda} \\right\\} $",
                    "Epochs"))%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')
