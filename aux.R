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

                                        # auxiliary functions
col.with.names <- function(tab, col) {
  v <- tab[,col];
  names(v) <- rownames(tab);
  return(v);
}

# return vector of matching assignments
inv.match <- function(cand, ref) {
  matrix.match <- function(cand) {
    n <- length(cand);
    my.mat <- matrix(1, n, n);
    colnames(my.mat) <- rownames(my.mat) <- names(cand);
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        my.mat[j,i] <- my.mat[i,j] <- cand[i] == cand[j];
      }
    }
    return(my.mat);
  }
  cand <- cand[!is.na(cand)];
  m1 <- matrix.match(ref[names(cand)]);
  m2 <- matrix.match(cand);
  v <- rep(NA, length(ref));
  names(v) <- names(ref);
  v[names(cand)] <- apply(m1==m2, 2, sum)/length(cand);
  return(v);
}


