# Autoregressive Model for stochastic processes
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-24
#

"""
Auto-regressive model for stochastic processes
"""

from Scientific.Functions.Interpolation import InterpolatingFunction
from Scientific.Functions.Polynomial import Polynomial
from Scientific.Functions.Rational import RationalFunction
from Scientific import N
import copy

class AutoRegressiveModel:

    """Auto-regressive model for stochastic process

    This implementation uses the Burg algorithm to obtain the
    coefficients of the AR model.
    """

    def __init__(self, order, data, delta_t=1):
        """
        @param order: the order of the model
        @type order: C{int}
        @param data: the time series
        @type data: sequence of C{float} or C{complex}
        @param delta_t: the sampling interval for the time series
        @type delta_t: C{float}
        """
        self.order = order
        self.delta_t = delta_t
        self._poles = None
        self._findCoefficients(data)
        self._setTrajectory(data)

    def _findCoefficients(self, data):
        e = data
        b = data
        a = N.array([1.])
        parcor = []
        sigsq = N.add.reduce(abs(data)**2)/len(data)
        self.variance = sigsq

        for r in range(self.order):
            er = e[1:]
            br = b[:-1]
            g = 2.*N.add.reduce(er*N.conjugate(br)) / \
                N.add.reduce(er*N.conjugate(er)+br*N.conjugate(br))
            parcor.append(g)
            e = er-g*br
            b = br-N.conjugate(g)*er
            a = N.concatenate((a, [0.]))
            a = a - g*N.conjugate(a[::-1])
            sigsq = sigsq*(1-abs(g)**2)

        self.coeff = -a[self.order:0:-1]
        self.parcor = N.array(parcor)
        self.sigsq = sigsq
        self.sigma = N.sqrt(sigsq)

    def _setTrajectory(self, data):
        self.trajectory = copy.copy(data[-self.order:])

    def predictStep(self):
        """
        Calculates the linear prediction of the next step in the series.
        This step is appended internally  to the current
        trajectory, making it possible to call this method repeatedly
        in order to obtain a sequence of predicted steps.
        
        @returns: the predicted step
        @rtype: C{float} or C{complex}
        """
        next = N.add.reduce(self.coeff*self.trajectory)
        self.trajectory[:-1] = self.trajectory[1:]
        self.trajectory[-1] = next
        return next

    def spectrum(self, omega):
        """
        @param omega: the angular frequencies at which the spectrum
        is to be evaluated
        @type omega: C{Numeric.array} of C{float}
        @returns: the frequency spectrum of the process
        @rtype: C{Numeric.array} of C{float}
        """
        sum = 1.
        for i in range(1, len(self.coeff)+1):
            sum = sum - self.coeff[-i]*N.exp(-1j*i*self.delta_t*omega)
        s = 0.5*self.delta_t*self.sigsq/(sum*N.conjugate(sum)).real
        return InterpolatingFunction((omega,), s)

    def poles(self):
        """
        @returns: the poles of the model in the complex M{z}-plane
        @rtype: C{Numeric.array} of C{complex}
        """
        if self._poles is None:
            from Scientific.LA import eigenvalues
            n = len(self.coeff)
            if n == 1:
                self._poles = self.coeff
            else:
                a = N.zeros_st((n, n), self.coeff)
                a[1:, :-1] = N.identity(n-1)
                a[:, -1] = self.coeff
                self._poles = eigenvalues(a)
        return self._poles

    def correlation(self, nsteps):
        """
        @param nsteps: the number of time steps for which the autocorrelation
        function is to be evaluated
        @type nsteps: C{int}
        @returns: the autocorrelation function of the process as estimated
        from the AR model
        @rtype: L{Scientific.Functions.Interpolation.InterpolatingFunction}
        """
        poles = self.poles()
        cpoles = N.conjugate(poles)
        x = 0.
        exponents = N.arange(self.order-1, nsteps+self.order-1)
        for i in range(len(poles)):
            pole = poles[i]
            factor = N.multiply.reduce((pole-poles)[:i]) * \
                     N.multiply.reduce((pole-poles)[i+1:]) * \
                     N.multiply.reduce((pole-1./cpoles))
            try:
                x = x + pole**exponents / factor
            except OverflowError:
                # happens with some Python versions on some systems
                power = N.zeros(exponents.shape, N.Complex)
                for i in range(len(exponents)):
                    try:
                        power[i] = pole**exponents[i]
                    except ValueError:
                        pass
                x = x + power/factor
        cf = -self.sigsq*x/N.conjugate(self.coeff[0])
        if not _isComplex(self.coeff):
            cf = _realPart(cf)
        return InterpolatingFunction((self.delta_t*N.arange(nsteps),), cf)

    def memoryFunctionZ(self):
        """
        @returns: the M{z}-transform of the process' memory function
        @rtype: L{Scientific.Function.Rational.RationalFunction}
        """
        poles = self.poles()
        cpoles = N.conjugate(poles)
        coeff0 = N.conjugate(self.coeff[0])
        beta = N.zeros((self.order,), N.Complex)
        for i in range(self.order):
            pole = poles[i]
            beta[i] = -(self.sigsq*pole**(self.order-1)/coeff0) / \
                      (N.multiply.reduce((pole-poles)[:i]) *
                       N.multiply.reduce((pole-poles)[i+1:]) *
                       N.multiply.reduce(pole-1./cpoles) *
                       self.variance)
        beta = beta/N.sum(beta)
        sum = 0.
        for i in range(self.order):
            sum = sum + RationalFunction([beta[i]], [-poles[i], 1.])
        mz = (1./sum+Polynomial([1., -1.]))/self.delta_t**2
        if not _isComplex(self.coeff):
            mz.numerator.coeff = _realPart(mz.numerator.coeff)
            mz.denominator.coeff = _realPart(mz.denominator.coeff)
        return mz

    def memoryFunctionZapprox(self, den_order):
        """
        @param den_order: 
        @type den_order: C{int}
        @returns: an approximation to the M{z}-transform of the process'
        memory function that correponds to an expansion of the
        denominator up to order den_order
        @rtype: L{Scientific.Function.Rational.RationalFunction}
        """
        poles = self.poles()
        cpoles = N.conjugate(poles)
        coeff0 = N.conjugate(self.coeff[0])
        beta = N.zeros((self.order,), N.Complex)
        for i in range(self.order):
            pole = poles[i]
            beta[i] = -(self.sigsq*pole**(self.order-1)/coeff0) / \
                      (N.multiply.reduce((pole-poles)[:i]) *
                       N.multiply.reduce((pole-poles)[i+1:]) *
                       N.multiply.reduce(pole-1./cpoles) *
                       self.variance)
        beta = beta/N.sum(beta)
        den_coeff = []
        for i in range(den_order):
            sum = 0.
            for j in range(self.order):
                sum += beta[j]*poles[j]**i
            den_coeff.append(sum)
        den_coeff.reverse()
        mz = (RationalFunction(den_order*[0.] + [1.], den_coeff)
              + Polynomial([1., -1.]))/self.delta_t**2
        if not _isComplex(self.coeff):
            mz.numerator.coeff = _realPart(mz.numerator.coeff)
            mz.denominator.coeff = _realPart(mz.denominator.coeff)
        return mz

    def memoryFunction(self, nsteps):
        """
        @param nsteps: the number of time steps for which the memory
        function is to be evaluated
        @type nsteps: C{int}
        @returns: the memory function of the process as estimated
        from the AR model
        @rtype: L{Scientific.Functions.Interpolation.InterpolatingFunction}
        """
        mz = self.memoryFunctionZapprox(nsteps+self.order)
        mem = mz.divide(nsteps-1)[0].coeff[::-1]
        if len(mem) == nsteps+1:
            mem = mem[1:]
        mem[0] = 2.*_realPart(mem[0])
        time = self.delta_t*N.arange(nsteps)
        return InterpolatingFunction((time,), mem)

    def frictionConstant(self):
        """
        @returns: the friction constant of the process, i.e. the
        integral over the memory function
        """
        poles = self.poles()
        cpoles = N.conjugate(poles)
        coeff0 = N.conjugate(self.coeff[0])
        beta = N.zeros((self.order,), N.Complex)
        for i in range(self.order):
            pole = poles[i]
            beta[i] = -(self.sigsq*pole**(self.order-1)/coeff0) / \
                      (N.multiply.reduce((pole-poles)[:i]) *
                       N.multiply.reduce((pole-poles)[i+1:]) *
                       N.multiply.reduce(pole-1./cpoles) *
                       self.variance)
        beta = beta/N.sum(beta)
        sum = 0.
        for i in range(self.order):
            sum = sum + beta[i]/(1.-poles[i])
        if not _isComplex(self.coeff):
            sum = _realPart(sum)
        return 1./(sum*self.delta_t)


class AveragedAutoRegressiveModel(AutoRegressiveModel):

    """Averaged auto-regressive model for stochastic process

    An averaged model is constructed by averaging the model
    coefficients of several auto-regressive models of the same
    order. An averaged model is created empty, then individual
    models are added.
    """

    def __init__(self, order, delta_t):
        """
        @param order: the order of the model
        @type order: C{int}
        @param delta_t: the sampling interval for the time series
        @type delta_t: C{float}
        """
        self.order = order
        self.delta_t = delta_t
        self.weight = 0.
        self.coeff = N.zeros((order,), N.Float)
        self.sigsq = 0.
        self.variance = 0.
        self.sigma = 0.
        self._poles = None

    def add(self, model, weight=1):
        """
        Adds the coefficients of an autoregressive model to the average.

        @param model: an autoregressive model
        @type model: L{AutoRegressiveModel}
        @param weight: the weight of the model in the average
        @type weight: C{float}
        @raise ValueError: if the order of the model does not match the
        order of the average model
        """
        if self.order != model.order:
            raise ValueError("model orders not equal")
        nw = self.weight + weight
        self.coeff = (self.weight*self.coeff + weight*model.coeff)/nw
        self.sigsq = (self.weight*self.sigsq + weight*model.sigsq)/nw
        self.sigma = N.sqrt(self.sigsq)
        self.variance = (self.weight*self.variance + weight*model.variance)/nw
        self.weight = nw
        self._poles = None


# Check if data is complex
def _isComplex(x):
    try:
        x.imag
        return 1
    except (AttributeError, ValueError):
        return 0

# Return real part
def _realPart(x):
    try:
        return x.real
    except (AttributeError, ValueError):
        return x

if __name__ == '__main__':

    import FFT
    def AutoCorrelationFunction(series):
        n = 2*len(series)
        FFTSeries = FFT.fft(series,n,0)
        FFTSeries = FFTSeries*N.conjugate(FFTSeries)
        FFTSeries = FFT.inverse_fft(FFTSeries,len(FFTSeries),0)
        return FFTSeries[:len(series)]/(len(series)-N.arange(len(series)))

    from MMTK.Random import gaussian
    from Scientific.Statistics import mean
    from Scientific.IO.ArrayIO import readArray
    from Gnuplot import plot
    from RandomArray import random
    dt = 1.
    t = dt*N.arange(500)
    if 1:
        data = N.sin(t) + N.cos(3.*t) + 0.1*(random(len(t))-0.5)
        data = data + 0.1j*(random(len(t))-0.5)
    if 0:
        data = [0.]
        for i in range(500+len(t)-1):
            data.append(mean(data[-500:]) + gaussian(0., 0.1))
        data = N.exp(1j*N.array(data[500:]))

    if 0:
        #data = readArray('~/scientific/Test/data')
        string = open('/users1/hinsen/scientific/Test/data').read()[4:]
        data = N.array(eval(string))
        data = data[:,0]
    model = AutoRegressiveModel(20, data, dt)
    print model.coeff
    print model.poles()
    c = model.correlation(200)
    cref = InterpolatingFunction((t,), AutoCorrelationFunction(data))[:200]
    m = model.memoryFunction(200)
    s = model.spectrum(N.arange(0., 5., 0.01))
    #plot(c.real, cref.real); plot(c.imag, cref.imag)
    print model.frictionConstant(), model.memoryFunctionZ()(1.), m.definiteIntegral()
    #plot(m.real, m.imag)
    plot(m)
