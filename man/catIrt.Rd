\name{catIrt}
\alias{catIrt}
\alias{summary.catIrt}
\alias{plot.catIrt}
\alias{print.catIrt}
\title{Simulate Computerized Adaptive Tests (CATs)}
\description{
\code{catIrt} simulates Computerized Adaptive Tests (CATs) given a vector/matrix of
responses or a vector of ability values, a matrix of item parameters, and several
item selection mechanisms, estimation procedures, and termination criteria.
}

\usage{
catIrt(params, mod = c("brm", "grm"),
       resp = NULL,
       thetas = NULL,
       catStart = list(init.thet = 0, step.size = 3,
                       select = c("random", "FI", "KL"),
                       at = c("theta", "bounds"),
                       n.select = 5,
                       score = c("fixed", "step", "random", "WLE", "BME", "EAP"),
                       n.it = 5, leave.after.MLE = FALSE),
       catMiddle = list(select = c("random", "FI", "KL"),
                        at = c("theta", "bounds"),
                        n.select = 5,
                        score = c("MLE", "WLE", "BME", "EAP"), int = c(-6, 6),
                        expos = c("none", "SH")),
       catTerm = list(term = c("fixed", "var", "class"),
                      score = c("MLE", "WLE", "BME", "EAP"),
                      n.min = 5, n.max = 50,
                      v.term = .25,
                      c.term = list(method = c("SPRT", "GLR", "CI"),
                                    bounds = c(-1, 1),
                                    categ = c(0, 1, 2),
                                    delta = .1,
                                    alpha = .05, beta = .05,
                                    conf.lev = .95)),
       ddist = dnorm,
       progress = TRUE, \dots)
\method{summary}{catIrt}(object, group = TRUE, ids = "none", \dots)
\method{plot}{catIrt}(x, which = "all", ids = "none", 
     CI = .95, legend = TRUE, ask = TRUE, \dots)
}

\arguments{
  \item{object, x}{a \code{catIrt} object.
}
  \item{params}{\bold{numeric:} a matrix of item parameters.  If specified as a matrix,
                the rows must index the items, and the columns must designate the item
                parameters.  For the binary response model, \command{params} must either
                be a 3-column matrix (if not using item exposure control), or a 4-column
                matrix (with Sympson-Hetter parameters as the last column if using item
                exposure control).  See \bold{Details} for more information.
}
  \item{mod}{\bold{character:} a character string indicating the IRT model.  Current support
             is for the 3-parameter binary response model (\option{"brm"}),
             and Samejima's graded response model (\option{"grm"}).  The contents
             of \command{params} must match the designation of \command{mod}.  If \command{mod} is
             left blank, it will be designated the class of \command{resp} (if \command{resp} inherits
             either \option{"brm"} or \option{"grm"}), and if that fails, it will ask the user (if in
             interactive mode) or error.
}
  \item{resp}{\bold{numeric:} either a \eqn{N \times J} matrix (where \eqn{N} indicates the
              number of simulees and \eqn{J} indicates the number of items), a \eqn{J}
              length vector (if there is only one simulee), or NULL if specifying \command{thetas}.
              For the binary response model (\option{"brm"}), \command{resp} must solely contain 0s
              and 1s.  For the graded response model (\option{"grm"}), \command{resp} must solely contain
              integers \eqn{1, \ldots, K}, where \eqn{K} is the number of categories, as
              indicated by the dimension of \command{params}.
}
  \item{thetas}{\bold{numeric:} either a \eqn{N}-dimensional vector (where \eqn{N} indicates the
                number of simulees) or NULL if specifying \command{resp}.
}
  \item{catStart}{\bold{list:} a list of options for starting the CAT including:

  \enumerate{
    \item \code{init.thet}: a scalar or vector of initial starting estimates of \eqn{\theta}.
           If \command{init.thet} is a scalar, every simulee will have the same starting value.
           Otherwise, simulees will have different starting values based on the respective element
           of \command{init.thet}.
    \item \code{step.size}: a scalar indicating how much to increment or decrement the
           estimate of \eqn{\theta} if \command{score} is set to \option{"step"}.
    \item \code{select}: a character string indicating the item selection method for the 
           first few items.  Items can be selected either through maximum Fisher information
           (\option{"FI"}) at a point, Kullback-Leibler Divergence (\option{"KL"}) at
           a point, or randomly (\option{"random"}).
    \item \code{at}: a character string indicating where to select items.  If \command{select}
           is \option{"FI"} and \command{at} is \option{"theta"}, then items will be selected
           to maximize Fisher information at the proximate \eqn{\theta} estimates.
    \item \code{n.select}: an \emph{integer} indicating the number of items to select at one time.
           For instance, if \command{select} is \option{"FI"}, \command{at} is \option{"theta"}, and
           \command{n.select} is 5, the item choosing function will randomly select between the top
           \emph{5} items that maximize expected Fisher information at proximate \eqn{\theta} estimates.
    \item \code{score}: a character string indicating the \eqn{\theta} estimation method.  As
           of now, the options for scoring the first few items are \option{"fixed"} (at \command{init.thet}),
           \option{"step"} (by adding or subtracting \command{step.size} \eqn{\theta} estimates after
           each item), Weighted Likelihood Estimation (\option{"WLE"}), Bayesian Modal Estimation (\option{"BME"}),
           and Expected A-Posteriori Estimation (\option{"EAP"}).  The latter two allow user specified
           prior distributions through density (\code{d...}) functions.  See \code{\link{mleEst}} for more
           information.
    \item \code{n.it}: a scalar indicating the number of items that are used for each 
           simulee at the beginning of the CAT.  After \option{n.it} reaches the specified value,
           the CAT will shift to the middle set of parameters.
    \item \code{leave.after.MLE}: a logical indicating whether to skip the remainder of the starting
           items if the user has a mixed response pattern and/or a finite maximum likelihood estimate
           of \eqn{\theta} can be achieved.
  }

}
  \item{catMiddle}{\bold{list:} a list of options for selecting/scoring during the middle of the CAT,
                  including:

  \enumerate{
    \item \code{select}: a character string indicating the item selection method for the 
		   remaining items.  See \command{select} in \command{catStart} for an explanation
		   of the options.
	\item \code{at}: a character string indicating where to select items.  See \command{select} in
	       \command{catStart} for an explanation of the options.
	\item \code{n.select}: an \emph{integer} indicating the number of items to select at one time.
	\item \code{score}: a character string indicating the \eqn{\theta} estimation method.  As
           of now, the options for scoring the remaining items are Maximum Likelihood Estimation (\option{"MLE"}),
           Weighted Likelihood Estimation (\option{"WLE"}), Bayesian Modal Estimation (\option{"BME"}),
           and Expected A-Posteriori Estimation (\option{"EAP"}).  The latter two allow user specified
           prior distributions through density (\code{d...}) functions.  See \code{\link{mleEst}} for more
           information.
    \item \code{int}: a 2-element integer vector indicating the minimum and maximum that
		   \eqn{\theta} should be estimated in the course of the CAT.
	\item \code{expos}: a character string indicating whether no item exposure controls should be
	       implemented (\option{"none"}) or whether the CAT should use Sympson-Hetter exposure
	       controls (\option{"SH"}).  If (and only if) \command{expos} is equal to \option{"SH"},
	       the last column of the parameter matrix should indicate the probability of an item
	       being administered given that it is selected.
  }
}
  \item{catTerm}{\bold{list:} a list of options for stopping/terminating the CAT, including:

  \enumerate{
    \item \code{term}: a scalar/vector indicating the termination criterion/criteria.
	      CATs can be terminated either through a fixed number of items
	      (\option{"fixed"}) declared through the \command{n.max} argument; after a
	      simulee's SEM dips below a threshold (\option{"var"}) declared through
	      the \command{v.term} argument; and/or when a simulee falls into a category.
          If more than one termination criteria is selected, the CAT will terminate after
          successfully satisfying the \emph{first} of those for a given simulee.
	\item \code{score}: a character string indicating the \eqn{\theta} estimation method for all
	      of the responses in the bank.  \command{score} is used to estimate \eqn{\theta} given
	      the entire bank of item responses and parameter set.  If the theta estimated using
	      all of the responses is far away from \eqn{\theta}, the size of the item bank is probably
	      too small.  The options for \command{score} in \command{catTerm} are identical to the
	      options of \command{score} in \command{catMiddle}.
    \item \code{n.min}: an \emph{integer} indicating the minimum number of items that a simulee
          should "take" before \emph{any} of the termination criteria are checked.
    \item \code{n.max}: an \emph{integer} indicating the maximum number of items to administer
          before terminating the CAT.
    \item \code{v.term}: a scalar indicating the maximum SEM of a simulee before terminating the
           CAT \bold{only if} \command{term} is \option{"var"}.
    \item \code{c.term}: a list indicating the parameters of a classification CAT, only if
           \command{term} is \option{"class"} or any of the selection methods are \command{at}
           one or more \option{"bounds"}, including:

    \enumerate{
      \item \code{method}: a scalar indicating the method used for a classification CAT.  As of
            now, the classification CAT options are the Sequential Probability Ratio Test (\option{"SPRT"}),
            the Generalized Likelihood Ratio (\option{"GLR"}), or the Confidence Interval method (\option{"CI"}).
      \item \code{bounds}: a scalar, vector, or matrix of classification bounds.  If specified as a
             scalar, there will be one bound for each simulee at that value.  If specified as a
             \eqn{N}-dimensional vector, there will be one bound for each simulee.  If specified as a
             \eqn{k < N}-dimensional vector, there will be \eqn{k} bounds for each simulee at those values.
              And if specified as a \eqn{N \times k}-element matrix, there will be \eqn{k} bounds for each
              simulee.
      \item \code{categ}: a vector indicating the names of the categories into which the simulees
             should be classified.  The length of \command{categ} should be one greater than the length
             of \command{bounds}.
      \item \code{delta}: a scalar indicating the half-width of an indifference region when performing
             an SPRT-based classification CAT or selecting items by Kullback-Leibler divergence.  See
             Eggen (1999) and \code{\link{KL}} for more information.
      \item \code{alpha}: a scalar indicating the specified Type I error rate for performing an SPRT-
             based classification CAT.
      \item \code{beta}: a scalar indicating the specified Type II error rate for performing an SPRT-
             based classification CAT.
      \item \code{conf.lev}: a scalar between 0 and 1 indicating the confidence level used when performing
            a confidence-based (\option{"CI"}) classification CAT.
    }

  }

}        
  \item{ddist}{\bold{function:} a function indicating how to calculate prior densities
               for Bayesian estimation.  For instance, if you wish to specify a normal
               prior, \code{ddist = dnorm}, and if you wish to specify a uniform prior,
               \code{ddist = dunif}.  Note that it is standard in R to use \code{d}\ldots
               to indicate a density.
}
  \item{which}{\bold{numeric:} a scalar or vector of integers between 1 and 4, indicating
               which plots to include.  The plots are as follows:
  \enumerate{
    \item Bank Information
    \item Bank SEM
    \item CAT Information
    \item CAT SEM
}
               \command{which} can also be "none", in which case \command{plot.catIrt} will
                not plot any information functions, or it can be "all", in which case
                \command{plot.catIrt} will plot all four information functions.
}
  \item{group}{\bold{logical:} TRUE or FALSE indicating whether to display a summary at the
               group level.
}
  \item{ids}{\bold{numeric:} a scalar or vector of integers between 1 and the number of
             simulees indicating which simulees to plot and/or summarize their CAT
             process and \emph{all} of their \eqn{\theta} estimates.  \command{ids} can
             also be "none" (or, equivalently, NULL) or "all".
}
  \item{CI}{\bold{numeric:} a scalar between 0 and 1 indicating the desired confidence
             level plotted for the individual \eqn{\theta} estimates.
}
  \item{legend}{\bold{logical:} TRUE or FALSE indicating whether the plot function should
                display a legend on the plot.
}
  \item{ask}{\bold{logical:} TRUE or FALSE indicating whether the plot function should ask
             between plots.
}
  \item{progress}{\bold{logical:} TRUE or FALSE indicating whether the \command{catIrt} function
                  should display a progress bar during the CAT.
}
  \item{\dots}{arguments passed to \command{ddist} or \command{plot.catIrt}, usually distribution
               parameters identified by name or graphical parameters.
}

}
\details{
The function \code{catIrt} performs a post-hoc computerized adaptive test (CAT),
with a variety of user specified inputs.  For a given person/simulee (e.g. simulee \eqn{i}),
a CAT represents a simple set of stages surrounded by a \command{while} loop
(e.g. Weiss and Kingsbury, 1984):

\itemize{
  \item Item Selection: The next item is chosen based on a pre-specified criterion/criteria.
        For example, the classic item selection mechanism is picking an item such that it
        maximizes Fisher Information at the current estimate of \eqn{\theta_i}.  Frequently,
        content balancing, item constraints, or item exposure will be taken into consideration
        at this point (aside from solely picking the "best item" for a given person).  More
        sophisticated item constraints are not implemented in the current edition of \code{catIrt}.
        See \code{\link{FI}} and \code{\link{KL}} for current item selection methods.
  \item Estimation: \eqn{\theta_i} is estimated based on updated information, usually relating
        to the just-selected item and the response associated with that item. In
        a post-hoc CAT, all of the responses already exist, but in a standard CAT, "item administration"
        would be between "item selection" and "estimation."  The classic estimation mechanism
        is estimating \eqn{\theta_i} based off of maximizing the likelihood given parameters and a set
        of responses.  Other estimation mechanisms correct for bias in the maximum likelihood
        estimate or add a prior information (such as a prior distribution of \eqn{\theta}).
        If an estimate is untenable (i.e. it returns a non-sensical value or \eqn{\infty}), the estimation
        procedure needs to have an alternative estimation mechanism.  See \code{\link{mleEst}} for
        current estimation methods.
  \item Termination: Either the test is terminated based on a pre-specified criterion/critera,
        or no termination criteria is satisfied, in which case the loop repeats.  The standard
        termination criteria involve a fixed criterion (e.g. administering only 50 items),
        or a variable criterion (e.g. continuing until the observed SEM is below .3).  Other
        termination criteria relate to cut-point tests (e.g. certification tests, classification tests),
        that depend not solely on ability but on whether that ability is estimated to exceed a threshold.
        \code{catIrt} terminates classification tests based on either the
        Sequential Probability Ratio Test (SPRT) (see Eggen, 1999), the Generalized Likelihood Ratio (GLR)
        (see Thompson, 2009), or the Confidence Interval Method (see Kingsbury & Weiss, 1983).  Essentially,
        the SPRT compares the ratio of two likelihoods (e.g. the likelihood of the data given being in one category
        vs the likelihood of the data given being in the other category, as defined by \eqn{B + \delta} and \eqn{B - \delta}
        (where \eqn{B} separates the categories and \eqn{\delta} is the halfwidth of the indifference region)
        and compares that ratio with a ratio of error rates (\eqn{\alpha} and \eqn{\beta}) (see Wald, 1945).  The
        GLR uses the maximum likelihood estimate in place of either \eqn{B + \delta} or \eqn{B - \delta},
        and the confidence interval method terminates a CAT if the confidence interval surrounding an estimate
        of \eqn{\theta} is fully within one of the categories.
}

The CAT estimates \eqn{\theta_{i1}} (an initial point) based on \command{init.thet}, 
and terminates the entire simulation after sequentially terminating each simulee's CAT.
}

\value{
The function \code{catIrt} returns a list (of class "catIrt") with the following elements:
  \item{cat_theta}{a vector of final CAT \eqn{\theta} estimates.
}
  \item{cat_categ}{a vector indicating the final classification of each simulee in the CAT.  If
                   \command{term} is \emph{not} \option{"class"}, \code{cat_categ} will be a vector
                   of NA values.
}
  \item{cat_info}{a vector of observed Fisher information based on the final CAT \eqn{\theta}
                  estimates and the item responses.
}
  \item{cat_sem}{a vector of observed SEM estimates based on the final CAT \eqn{\theta} estimates
                 and the item responses.
}
  \item{cat_length}{a vector indicating the number of items administered to each simulee in the CAT
}
  \item{cat_term}{a vector indicating how each CAT was terminated.
}
  \item{tot_theta}{a vector of \eqn{\theta} estimates given the entire item bank.
}
  \item{tot_categ}{a vector indicating the classification of each simulee given the entire item bank.
}
  \item{tot_info}{a vector of observed Fisher information based on the entire item bank worth
                  of responses.
}
  \item{tot_sem}{a vector of observed SEM estimates based on the entire item bank worth of responses.
}
  \item{true_theta}{a vector of true \eqn{\theta} values if specified by the user.
}
  \item{true_categ}{a vector of true classification given \eqn{\theta}.
}
  \item{full_params}{the full item bank.
}
  \item{full_resp}{the full set of responses.
}
  \item{cat_indiv}{a list of \eqn{\theta} estimates, observed SEM, observed information, the responses
                   and the parameters chosen for each simulee over the entire CAT.
}
  \item{mod}{a list of model specifications, as designated by the user, so that the
             CAT can be easily reproduced.
}

}

\note{
Both \code{summary.catIrt} and \code{plot.catIrt} return different objects than the original
\code{catIrt} function.  \code{summary.catIrt} returns summary labeled summary statistics, and
\code{plot.catIrt} returns evaluation points (\eqn{x} values, information, and SEM) for each
of the plots.  Moreover, if in interactive mode and missing parts of the \code{catStart}, \code{catMiddle},
or \code{catTerm} arguments, the \code{catIrt} function will interactively ask for each of those
and return the set of arguments in the "catIrt" object.
}

\references{
Eggen, T. J. H. M.  (1999).  Item selection in adaptive testing with the sequential probability ratio test.  \emph{Applied Psychological Measurement}, \emph{23}, 249 -- 261.

Kingsbury, G. G., & Weiss (1983).  A comparison of IRT-based adaptive mastery testing and a sequential mastery
testing procedure.  In D. J. Weiss (Ed.), \emph{New horizons in testing: Latent trait test theory and computerized adaptive testing} (pp. 257--283).  New York, NY: Academic Press.

Thompson, N. A. (2009).  Using the generalized likelihood ratio as a termination criterion.  In D. J. Weiss (Ed.),
\emph{Proceedings of the 2009 GMAC conference on computerized adaptive testing}.

Wainer, H. (Ed.). (2000). \emph{Computerized Adaptive Testing: A Primer (2nd Edition)}. Mahwah, NJ: ELawrence Erlbaum Associates.

Wald, A.  (1945).  Sequential tests of statistical hypotheses.  \emph{Annals of Mathematical Statistics}, \emph{16}, 117 -- 186.

Weiss, D. J., & Kingsbury, G. G. (1984). Application of computerized adaptive testing to educational problems. \emph{Journal of Educational Measurement}, \emph{21}, 361-375.
}

\author{
Steven W. Nydick
}

\seealso{
\code{\link{FI}}, \code{\link{KL}}, \code{\link{mleEst}}, \code{\link[catR]{randomCAT}},
\code{\link{simIrt}}
}

\examples{
#########################
# Binary Response Model #
#########################
set.seed(888)
# generating random theta:
thetas <- rnorm(50)
# generating an item bank under a 2-parameter binary response model:
b.params <- cbind(a = runif(100, .5, 1.5), b = rnorm(100, 0, 2), c = 0)
# simulating responses:
b.resp <- simIrt(thetas = thetas, params = b.params, mod = "brm")$resp


## CAT 1 ##
# the typical, classic post-hoc CAT:
catStart1 <- list(init.thet = 0, step.size = 3,
                  select = "FI", at = "theta",
                  n.select = 4, score = "step",
                  n.it = 5, leave.after.MLE = TRUE)
catMiddle1 <- list(select = "FI", at = "theta",
                   n.select = 1, score = "MLE",
                   int = c(-6, 6), expos = "none")
catTerm1 <- list(term = "fixed", n.min = 10, n.max = 50)

cat1 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle1,
               catTerm = catTerm1)

# we can print, summarize, and plot:
cat1                                        # prints theta because
                                            # we have fewer than
                                            # 200 simulees
summary(cat1, group = TRUE, ids = "none")   # nice summary!

summary(cat1, group = FALSE, ids = 1:4)     # summarizing people too! :)

par(mfrow = c(2, 2))
plot(cat1, ask = FALSE)               # 2-parameter model, so expected FI
                                      # and observed FI are the same
par(mfrow = c(1, 1))

# we can also plot particular simulees:
par(mfrow = c(2, 1))
plot(cat1, which = "none", ids = c(1, 30), ask = FALSE)
par(mfrow = c(1, 1))

\dontrun{

## CAT 2 ##
# using KL rather than FI to select items:
catStart2 <- catStart1
catMiddle2 <- catMiddle1
catTerm2 <- catTerm1

catMiddle2$select <- "KL"
catMiddle2$at <- "bounds"
catTerm2$c.term <- list(bounds = 0, delta = .1)
cat2 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart2,
               catMiddle = catMiddle2,
               catTerm = catTerm2)
cor(cat1$cat_theta, cat2$cat_theta)    # very close!


## CAT 3/4 ##
# using "var" rather than "fixed" to terminate:
catTerm1$term <- catTerm2$term <- "var"
catTerm1$v.term <- catTerm2$v.term <- .3
cat3 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle1,
               catTerm = catTerm1)
cat4 <- catIrt(params = b.params, mod = "brm",
			   resp = b.resp,
			   catStart = catStart2,
			   catMiddle = catMiddle2,
			   catTerm = catTerm2)

mean(cat3$cat_length - cat4$cat_length) # KL info results in slightly more items


## CAT 5/6 ##
# classification CAT with a boundary of 0 (with default classification stuff):
catTerm5 <- list(term = "class", n.min = 10, n.max = 50,
                 c.term = list(method = "SPRT",
                               bounds = 0, delta = .2,
                               alpha = .10, beta = .10))
cat5 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle1,
               catTerm = catTerm5)
cat6 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle2,
               catTerm = catTerm5)

# how many were classified correctly?
mean(cat5$cat_categ == cat5$tot_categ)

# using a different selection mechanism, we get the similar results:
mean(cat6$cat_categ == cat6$tot_categ)


## CAT 7 ##
# we could change estimation to EAP with the default (normal) prior:
catMiddle7 <- catMiddle1
catMiddle7$score <- "EAP"
cat7 <- catIrt(params = b.params, mod = "brm", # much slower!
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle7,
               catTerm = catTerm1)
cor(cat1$cat_theta, cat7$cat_theta)    # pretty much the same


## CAT 8 ##
# let's specify the prior as something strange:
cat8 <- catIrt(params = b.params, mod = "brm",
               resp = b.resp,
               catStart = catStart1,
               catMiddle = catMiddle7,
               catTerm = catTerm1,
               ddist = dchisq, df = 4)

cat8   # all positive values of "theta"


## CAT 9 ##
# finally, we can have:
#   - more than one termination criteria,
#   - individual bounds per person,
#   - simulating based on theta without a response matrix.
catTerm9 <- list(term = c("fixed", "class"),
                 n.min = 10, n.max = 50,
                 c.term = list(method = "SPRT",
                               bounds = cbind(runif(length(thetas), -1, 0),
                                              runif(length(thetas), 0, 1)),
                               delta = .2,
                               alpha = .1, beta = .1))
cat9 <- catIrt(params = b.params, mod = "brm",
               resp = NULL, thetas = thetas,
               catStart = catStart1,
               catMiddle = catMiddle1,
               catTerm = catTerm9)

summary(cat9)   # see "... with Each Termination Criterion"


#########################
# Graded Response Model #
#########################
# generating random theta
thetas <- rnorm(201)
# generating an item bank under a graded response model:
g.params <- cbind(a = runif(100, .5, 1.5), b1 = rnorm(100), b2 = rnorm(100),
                                           b3 = rnorm(100), b4 = rnorm(100))

# the graded response model is exactly the same, only slower!
cat10 <- catIrt(params = g.params, mod = "grm",
                resp = NULL, thetas = thetas,
                catStart = catStart1,
                catMiddle = catMiddle1,
                catTerm = catTerm1)

# if there is more than 200 simulees, it doesn't print individual thetas:
cat10
}

# play around with things - CATs are fun - a little frisky, but fun.
}
