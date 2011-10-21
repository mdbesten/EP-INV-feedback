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


# generate random inventor table
rinv <- function(n = 100, m = n/2, ratio = .5) {
                                        # n unique patent ids
  patent <- sample(10*n, n);
                                        # n person ids out of m potential unique
  person <- sample(m, n, replace = TRUE);
                                        # randomly group persons
  psz <- nlevels(as.factor(person));
  inv <- sample(ratio*psz, psz, replace = TRUE);
  names(inv) <- levels(as.factor(person));
                                        # todo: same person multi inventor
  return(data.frame(person = person,
                    patent = sample(10*n, n),
                    inventor = inv[as.character(person)]));
}

# generate alternative vector of inventor assignments
inv.reassign <- function(tab, ratio = .5) {
  psz <- nlevels(as.factor(tab$person));
  inv <- sample(ratio*psz, psz, replace = TRUE);
  names(inv) <- levels(as.factor(tab$person));
  tab$inventor <- inv[as.character(tab$person)];
  return(tab);
}

inv.reassign.vector <- function(tab, ratio = .9) {
  t2 <- inv.reassign(tab, ratio);
  v <- t2$inventor;
  names(v) <- rownames(t2);
  return(v);
}

# randomly select rows
rselect <- function(n, p) {
  return(sort(sample(1:n,n*p)));
}
