##Production des tableau de résultats


##Shallow 
tab <- data.frame(tab_shal_neur[,c(6,2,4,12)])
rownames(tab) <- NULL
library(kableExtra)

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

