#    Copyright: Matthijs den Besten
#
#    This file is part of EP-INV-feedback.
#
#    EP-INV-feedback is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Foobar is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with EP-INV-feedback.  If not, see <http://www.gnu.org/licenses/>.
#


source("aux.R");
source("random.R");

################################################################################
# simulate scenarios (explore use cases)                                       #
################################################################################

# random feedback on random table
# todo: introduce clock
rfeedback <- function(n = 20,
                      inv.ratio = .8,
                      download.proportions = runif(100, 1/n),
                      upload.proportions = runif(10, 1/n),
                      cand.ratios = runif(10, 0.9*inv.ratio,1.1*inv.ratio),
                      total.users = 10) {
 
                                        # initialize tables  
  inventor.table <- rinv(n, ratio = inv.ratio);
  download.table <- data.frame(progr = rownames(inventor.table),
                               downloads = numeric(nrow(inventor.table)));
  upload.table <- data.frame(progr = rownames(inventor.table),
                             ref = inventor.table$inventor);
  controversy.table <- data.frame(progr = rownames(inventor.table),
                                  controversies = numeric(nrow(inventor.table)))
  
                                        # update functions
  update.downloads <- function(range = rselect(n, 0.5),
                               dtab = download.table) {
    dtab$downloads[range] <- dtab$downloads[range]+1;
    return(dtab);
  }

  update.uploads <- function(inv.attr = inv.reassign.vector(inventor.table,.9),
                             uid = paste("cand",sample(1:total.users,1),
                               sep=""),
                             utab = upload.table) {
    if(!(uid %in% colnames(utab))) {
      utab[,uid] <- rep(NA, nrow(utab));
    }
    utab[names(inv.attr), uid] <- inv.attr;
    return(utab);
  }

  update.controversies <- function(inv.attr = col.with.names(upload.table,
                                     ncol(upload.table)),
                                   ref = col.with.names(inventor.table,
                                     "inventor"),
                                   ctab = controversy.table) {
    stopifnot(length(ref) == nrow(ctab));
    agreement.vector <- inv.match(inv.attr, ref);
    s <- !is.na(agreement.vector);
    ctab$controversies[s] <- ctab$controversies[s] + (1 - agreement.vector[s]);
    return(ctab);
  }


                                        # run simulation
                                        # todo: alternate upload/download steps
  for(d in download.proportions) {
    download.table <- update.downloads(rselect(n,d), download.table);
  }

  ref.assignment <- col.with.names(inventor.table, "inventor");
  for(i in 1:length(upload.proportions)) {
    input.selection <- inventor.table[rselect(n, upload.proportions[i]),];
    new.assignment <- inv.reassign.vector(input.selection, cand.ratios[i]);
    upload.table <- update.uploads(new.assignment,
                                   paste("u", sample(1:total.users,1), sep=""),
                                   upload.table);
    controversy.table <- update.controversies(new.assignment, ref.assignment,
                                              controversy.table);
  }
  
  
  return(list(ref = inventor.table,
              downloads = download.table,
              uploads = upload.table,
              controversies = controversy.table));
}

                                        # to be computed on the fly
get.operating.table <- function(tab = rfeedback()$uploads) {
  vector.match <- function(cand) {
    n <- length(cand);
    my.mat <- NULL;
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        my.mat <- rbind(my.mat,
                        data.frame(a = i, b = j, v = cand[i] == cand[j]));
      }
    }
    return(my.mat);
  }

  operating.controversies <- function(tab) {
    return(apply(tab[,-(1:2)], 1,
                 function(v) {
                   w <- v[!is.na(v)];
                   return(sum(w[1]!=w[-1])/(length(w)-1));
                 }));
  }

  otab <- vector.match(tab$ref);
  for(col in colnames(tab[,-c(1:2)])) {
    otab <- cbind(otab, vector.match(tab[,col])$v);
  }
  names(otab) <- c("progr", colnames(tab));
  otab[,"controversies"] <- operating.controversies(otab);

  return(otab);
}


aggregate.controversies <- function(tab = rfeedback()$uploads) {
  return(apply(sapply(colnames(tab[,-(1:2)]),
                      function(x) {
                        return(1 - inv.match(col.with.names(tab, x),
                                             col.with.names(tab, "ref")));
                      }), 1, sum, na.rm = TRUE));
}


 
