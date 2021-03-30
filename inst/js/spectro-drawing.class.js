

class SpectroDrawingClass {

	/**
	 * A handy class to draw a spectrogram
	 */

	///////////////////////////////////
	// start: class vars
	executed = false;
	PI = 3.141592653589793; // value : Math.PI
	TWO_PI = 6.283185307179586; // value : 2 * Math.PI
	totalMax = 0;
	dynRangeInDB = 50;
	myWindow = {
		BARTLETT: 1,
		BARTLETTHANN: 2,
		BLACKMAN: 3,
		COSINE: 4,
		GAUSS: 5,
		HAMMING: 6,
		HANN: 7,
		LANCZOS: 8,
		RECTANGULAR: 9,
		TRIANGULAR: 10
	};
	imgWidth = 0;
	imgHeight = 0;
	upperFreq = 0;
	lowerFreq = 0;
	pixelRatio = 1;
	heatMapColorAnchors = [
		[255, 0, 0],
		[0, 255, 0],
		[0, 0, 0]
	];
	samplesPerPxl = 0;
	sampleRate = 0;
	preEmphasisFilterFactor = 0.97;
	transparency = 0;
	drawHeatMapColors = false;
	N = 0;
	windowSizeInSecs = 0.01;
	audioBuffer = undefined;
	audioBufferChannels = 0;
	wFunction = 0;
	myFFT = undefined;
	pixelHeight = 1;
	internalalpha = 1;
	maxPsd = 0;
	HzStep = 0;
	paint = [];
	sin = undefined;
	cos = undefined;
	ceilingFreqFftIdx = 0;
	floorFreqFftIdx = 0;
	resultImgArr = undefined;
	m = undefined;

	// end: class vars
	//////////////////////////////////

	/////////////////////////////////
	// start: math helper functions

	// used by FFT
	toLinearLevel(dbLevel) {
		return Math.pow(10, (dbLevel / 10));
	};

	// calculate decadic logarithm
	log10(arg) {
		return Math.log(arg) / 2.302585092994046; // Math.log(x) / Math.LN10
	};

	// calculate magintude
	magnitude(real, imag) {
		return Math.sqrt((real * real) + (imag * imag));
	};
    
	
	/**
    * calculate the closest power of two that is
    * greater than the passed in number
    * @param num
    * @returns power of two value
    */
	calcClosestPowerOf2Gt(num) {
    	var curExp = 0;
        
        while (Math.pow(2, curExp) < num) {
            curExp = curExp + 1;
        }
        
        return (Math.pow(2, curExp));
        
    };



	// end: math helper functions
	////////////////////////////////

	///////////////////////////////////////////////////
	// start: FFT functions

	createSinAndCosTables (){
		var i, x;
		var n = this.N;
		this.m = parseInt((Math.log(n) / 0.6931471805599453), 10);
		if (n !== (1 << this.m)) { // Make sure n is a power of 2
			// console.log('ERROR : FFT length must be power of 2');
		}
		if (this.cos === undefined || n !== this.N) {

			// this means that the following is only executed 
			// when no COS table exists
			// or n changes

			this.cos = new Float32Array(n / 2); // precompute cos table
			for (x = 0; x < n / 2; x++) {
				this.cos[x] = Math.cos(-2 * this.PI * x / n);
			}
		}
		if (this.sin === undefined || n !== this.N) {

			// this means that the following is only executed 
			// when no COS table exists
			// or n changes 

			this.sin = new Float32Array(n / 2); // precompute sin table
			for (x = 0; x < n / 2; x++) {
				this.sin[x] = Math.sin(-2 * this.PI * x / n);
			}
		}
	}

	/**
	 * apply window function and pre-emphasis from idx 0 to length
	 * in buffer given
	 *
	 * @param type is the chosen window Function as enum
	 * @param alpha is the alpha value for Window Functions (default 0.16)
	 * @param buffer is the zero padded magnitude spectrum
	 * @param length is the length to in the buffer (starting at idx 0)
	 * to which to apply the window to. If the buffer is [x0, x1, x2, x4] and
	 * length is 2 the window will be applied to [x0, x1, x2] this is needed
	 * to only apply function to non-zero-padded values of magnitude spectrum.
	 * @return the windowed/pre-emphasised buffer
	 */
	applyWindowFuncAndPreemph(type, alpha, buffer, length) {
		// var length = buffer.length;
		let i = 0;
		this.alpha = alpha;
		switch (type) {
			case this.myWindow.BARTLETT:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionBartlett(length, i);
				}
				break;
			case this.myWindow.BARTLETTHANN:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionBartlettHann(length, i);
				}
				break;
			case this.myWindow.BLACKMAN:
				this.alpha = this.alpha || 0.16;
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionBlackman(length, i, alpha);
				}
				break;
			case this.myWindow.COSINE:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionCosine(length, i);
				}
				break;
			case this.myWindow.GAUSS:
				this.alpha = this.alpha || 0.25;
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionGauss(length, i, alpha);
				}
				break;
			case this.myWindow.HAMMING:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionHamming(length, i);
				}
				break;
			case this.myWindow.HANN:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionHann(length, i);
				}
				break;
			case this.myWindow.LANCZOS:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionLanczos(length, i);
				}
				break;
			case this.myWindow.RECTANGULAR:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionRectangular(length, i);
				}
				break;
			case this.myWindow.TRIANGULAR:
				for (i = 0; i < length; i++) {
					if (i > 0) {
						buffer[i] = this.applyPreEmph(buffer[i], buffer[i - 1]);
					}
					buffer[i] *= this.wFunctionTriangular(length, i);
				}
				break;
		}
		return buffer;
	};
	////////////////////////////////////
	// start: the windowing functions

	wFunctionBartlett (length, index) {
		return 2 / (length - 1) * ((length - 1) / 2 - Math.abs(index - (length - 1) / 2));
	};

	wFunctionBartlettHann (length, index) {
		return 0.62 - 0.48 * Math.abs(index / (length - 1) - 0.5) - 0.38 * Math.cos(this.TWO_PI * index / (length - 1));
	};

	wFunctionBlackman(length, index, alpha) {
		var a0 = (1 - alpha) / 2;
		var a1 = 0.5;
		var a2 = alpha / 2;
		return a0 - a1 * Math.cos(this.TWO_PI * index / (length - 1)) + a2 * Math.cos(4 * this.PI * index / (length - 1));
	};

	wFunctionCosine(length, index) {
		return Math.cos(this.PI * index / (length - 1) - this.PI / 2);
	};

	wFunctionGauss(length, index, alpha) {
		return Math.pow(Math.E, -0.5 * Math.pow((index - (length - 1) / 2) / (alpha * (length - 1) / 2), 2));
	};

	wFunctionHamming(length, index) {
		return 0.54 - 0.46 * Math.cos(this.TWO_PI * index / (length - 1));
	};

	wFunctionHann(length, index) {
		return 0.5 * (1 - Math.cos(this.TWO_PI * index / (length - 1)));
	};

	wFunctionLanczos(length, index) {
		var x = 2 * index / (length - 1) - 1;
		return Math.sin(this.PI * x) / (this.PI * x);
	};

	wFunctionRectangular() {
		return 1;
	};

	wFunctionTriangular(length, index) {
		return 2 / length * (length / 2 - Math.abs(index - (length - 1) / 2));
	};
	// end: the windowing functions
	///////////////////////////////////

	/**
	 * calculate and apply according pre-emphasis on sample
	 */
	applyPreEmph(curSample, prevSample) {
		return curSample - this.preEmphasisFilterFactor * prevSample;
	};

	// the FFT calculation
	fft (x, y) {
		// Bit-reverse
		var i, j, k, n1, n2, a, c, s, t1, t2;
		// Bit-reverse
		j = 0;
		n2 = this.N / 2;
		for (i = 1; i < this.N - 1; i++) {
			n1 = n2;
			while (j >= n1) {
				j = j - n1;
				n1 = n1 / 2;
			}
			j = j + n1;

			if (i < j) {
				t1 = x[i];
				x[i] = x[j];
				x[j] = t1;
				t1 = y[i];
				y[i] = y[j];
				y[j] = t1;
			}
		}

		// FFT
		n1 = 0;
		n2 = 1;

		for (i = 0; i < this.m; i++) {
			n1 = n2;
			n2 = n2 + n2;
			a = 0;

			for (j = 0; j < n1; j++) {
				c = this.cos[a];
				s = this.sin[a];
				a += 1 << (this.m - i - 1);

				for (k = j; k < this.N; k = k + n2) {
					t1 = c * x[k + n1] - s * y[k + n1];
					t2 = s * x[k + n1] + c * y[k + n1];
					x[k + n1] = x[k] - t1;
					y[k + n1] = y[k] - t2;
					x[k] = x[k] + t1;
					y[k] = y[k] + t2;
				}
			}
		}
	};

	// end: FFT functions
	///////////////////

// 	/////////////////////////////////
// 	// start: rendering function 

	/**
	 * interpolates a 3D color space and calculate accoring
	 * value on that plane
	 *
	 * @param minval is the minimum value to map to (number)
	 * @param maxval is the maximum value to map to (number)
	 * @param val is the value itself (number)
	 * @param colors is an array of arrays containing the colors
	 * to interpol. against (of the form: [[255, 0, 0],[0, 255, 0],[0, 0, 255]])
	 */
	convertToHeatmap = function (minval, maxval, val, colors) {
		let maxIndex = colors.length - 1;
		let v = (val - minval) / (maxval - minval) * maxIndex;
		let i1 = Math.floor(v);
		let i2 = Math.min.apply(null, [Math.floor(v) + 1, maxIndex]);
		let rgb1 = colors[i1];
		let rgb2 = colors[i2];
		let f = v - i1;
		let res = {
			'r': Math.floor(rgb1[0] + f * (rgb2[0] - rgb1[0])),
			'g': Math.floor(rgb1[1] + f * (rgb2[1] - rgb1[1])),
			'b': Math.floor(rgb1[2] + f * (rgb2[2] - rgb1[2]))
		};
		return (res);
	};


	/**
	 * draws a single line of the spectrogram into the imageResult array.
	 * by calculating the RGB value of the current pixel with:
	 * 255-(255*scaled)
	 * @param xIdx in the this.paint array
	 */
	drawVerticalLineOfSpectogram (xIdx) {

		// set upper boundary for linear interpolation
		var x1 = this.pixelHeight;
		var rgb, index, px, py;

		// value for first interpolation at lower boundry (height=0)

		// calculate the one sided power spectral density PSD (f, t) in Pa2/Hz
		// PSD(f) proportional to 2|X(f)|2 / (t2 - t1)
		var psd = (2 * Math.pow(this.paint[xIdx][1], 2)) / this.N;
		var psdLog = 10 * this.log10(psd / this.maxPsd);
		var scaledVal = ((psdLog + this.dynRangeInDB) / this.dynRangeInDB);
		if (scaledVal > 1) {
			scaledVal = 1;
		} else if (scaledVal < 0) {
			scaledVal = 0;
		}

		for (var i = 0; i < this.paint[xIdx].length; i++) {

			var y0 = scaledVal; // !!!! set y0 to previous scaled value

			// for each value in paint[] calculate pixelHeight interpolation points
			// x0=0
			// x1=pixelHeight
			// if(paint[i-1]<0) paint[i-1] = 1
			// y0=paint[i-1]    
			// y1=paint[i]


			// !!!! calculate next scaledValue [0...1] 
			psd = (2 * Math.pow(this.paint[xIdx][i], 2)) / this.N;
			psdLog = 10 * this.log10(psd / this.maxPsd);
			scaledVal = ((psdLog + this.dynRangeInDB) / this.dynRangeInDB);
			if (scaledVal > 1) {
				scaledVal = 1;
			}
			if (scaledVal < 0) {
				scaledVal = 0;
			}

			// !!!! set y1 to this scaled value
			var y1 = scaledVal;

			if (this.pixelHeight >= 1) {
				// do interpolation between y0 (previous scaledValue) and y1 (scaledValue now)
				for (var b = 0; b < this.pixelHeight; b++) {
					var y2 = y0 + (y1 - y0) / x1 * b;

					// calculate corresponding color value for interpolation point [0...255]
					// console.log(this.invert);
					if (this.invert) {
						rgb = Math.round(255 * y2);
					} else {
						rgb = 255 - Math.round(255 * y2);
					}

					// set internal image buffer to calculated & interpolated value
					px = Math.floor(xIdx);
					py = Math.floor(this.imgHeight - (this.pixelHeight * (i - 2) + b));

					index = (px + (py * this.imgWidth)) * 4;
					if (this.drawHeatMapColors) {
						if (!isNaN(rgb)) {
							var hmVals = this.convertToHeatmap(0, 255, rgb, this.heatMapColorAnchors);
							this.resultImgArr[index + 0] = hmVals.r;
							this.resultImgArr[index + 1] = hmVals.g;
							this.resultImgArr[index + 2] = hmVals.b;
							this.resultImgArr[index + 3] = this.transparency;

						} else {
							this.resultImgArr[index + 0] = rgb;
							this.resultImgArr[index + 1] = rgb;
							this.resultImgArr[index + 2] = rgb;
							this.resultImgArr[index + 3] = this.transparency;
						}

					} else {
						this.resultImgArr[index + 0] = rgb;
						this.resultImgArr[index + 1] = rgb;
						this.resultImgArr[index + 2] = rgb;
						this.resultImgArr[index + 3] = this.transparency;
					}
				}
			} else {
				rgb = 255 - Math.round(255 * y1);
				// set internal image buffer to calculated & interpolated value
				px = Math.floor(xIdx);
				py = Math.floor(this.imgHeight - (this.pixelHeight * (i - 2)));

				index = (px + (py * this.imgWidth)) * 4;
				this.resultImgArr[index + 0] = rgb;
				this.resultImgArr[index + 1] = rgb;
				this.resultImgArr[index + 2] = rgb;
				this.resultImgArr[index + 3] = global.transparency;
			}
		}
	};


	/**
	 * calculates Magnitude by
	 * - reading the current (defined with offset) data from localSoundBuffer
	 * - applying the current Window Function to the selected data
	 * - calculating the actual FFT
	 * - (and saving the biggest value in totalMax)
	 *
	 * @param offset calculated offset in PCM Stream
	 * @param windowSizeInSamples size of window in samples (actual samples -> not FFT length; rest zero-padded)
	 * @return magnitude spectrum as Float32Array
	 */
	calcMagnitudeSpectrum(offset, windowSizeInSamples) {
		// imaginary array of length N
		var imag = new Float32Array(this.N);

		// real array of length N
		var real = new Float32Array(this.N);

		// result array of length c - d
		var result = new Float32Array(this.ceilingFreqFftIdx - this.floorFreqFftIdx);

		// set real values by reading local sound buffer (this auto zeropads everything > windowSizeInSamples)
		for (var j = 0; j < windowSizeInSamples; j++) {
			real[j] = this.audioBuffer[offset + j];
		}

		// apply window function and pre-emphasis to non zero padded real 
		this.applyWindowFuncAndPreemph(this.wFunction, this.internalalpha, real, windowSizeInSamples);

		// calculate FFT over real and save to result
		this.fft(real, imag);

		// calculate magnitude for each spectral component 
		for (var low = 0; low <= this.ceilingFreqFftIdx - this.floorFreqFftIdx; low++) {
			result[low] = this.magnitude(real[low + this.floorFreqFftIdx], imag[low + this.floorFreqFftIdx]);
			if (this.totalMax < result[low]) {
				this.totalMax = result[low];
			}
		}
		return result;
	};

	/**
	 * initial function call for calculating and drawing Spectrogram
	 * input sample data comes from the buffer this.audioBuffer
	 * - first loop calculates magnitude spectra to draw (calcMagnitudeSpectrum())
	 * - second loop draws these values into the this.resultImgArr (drawVerticalLineOfSpectogram())
	 */
	_renderSpectrogram () {

		var windowSizeInSamples = this.sampleRate * this.windowSizeInSecs;

		// instance of FFT with windowSize N
		// this.myFFT = new this.FFT();

		// array holding FFT results paint[canvas width][canvas height]
		this.paint = new Array(this.imgWidth);

		// Hz per pixel height
		this.HzStep = (this.sampleRate / 2) / (this.N / 2);

		// upper Hz boundary to display
		this.ceilingFreqFftIdx = Math.ceil(this.upperFreq / this.HzStep);

		// lower Hz boundary to display
		this.floorFreqFftIdx = Math.floor(this.lowerFreq / this.HzStep); // -1 for value below display when lower>0

		// height between two interpolation points
		this.pixelHeight = this.imgHeight / (this.ceilingFreqFftIdx - this.floorFreqFftIdx - 2);

		// ugly hack in order to support PhantomJS < 2.0 testing
		if (typeof Uint8ClampedArray === 'undefined') {
			Uint8ClampedArray = Uint8Array;
		}
		// create new picture
		this.resultImgArr = new Uint8ClampedArray(Math.ceil(this.imgWidth * this.imgHeight * 4));
		// create sin & cos tables + m var (prob. better place that some place else!)
		this.createSinAndCosTables();

		// calculate i FFT runs, save result into paint and set maxPsd while doing so
		for (var i = 0; i < this.imgWidth; i++) {
			this.paint[i] = this.calcMagnitudeSpectrum(Math.round(i * this.samplesPerPxl), windowSizeInSamples);
			this.maxPsd = (2 * Math.pow(this.totalMax, 2)) / this.N;
		}

		// draw spectrogram on png image with canvas width
		// one column is drawn per drawVerticalLineOfSpectogram
		for (var j = 0; j < this.imgWidth; j++) {
			this.drawVerticalLineOfSpectogram(j);
		}

		return(this.resultImgArr.buffer);


	}

	// end: rendering function 
	//////////////////////////////

	//////////////////////////
	// communication functions


	/**
	 * function to handle messages events if used ad web worker
	 * @param e message event
	 */
	 renderSpectrogram (data) {
		if (data !== undefined) {
			var render = true;
			var renderError = '';
			if (data.windowSizeInSecs !== undefined) {
				this.windowSizeInSecs = data.windowSizeInSecs;
			} else {
				renderError = 'windowSizeInSecs';
				render = false;
			}
			if (data.fftN !== undefined) {
				this.N = data.fftN;
			} else {
				renderError = 'fftN';
				render = false;
			}
			if (data.alpha !== undefined) {
				this.internalalpha = data.alpha;
			} else {
				renderError = 'alpha';
				render = false;
			}
			if (data.upperFreq !== undefined) {
				this.upperFreq = data.upperFreq;
			} else {
				renderError = 'upperFreq';
				render = false;
			}
			if (data.lowerFreq !== undefined) {
				this.lowerFreq = data.lowerFreq;
			} else {
				renderError = 'lowerFreq';
				render = false;
			}
			if (data.samplesPerPxl !== undefined) {
				this.samplesPerPxl = data.samplesPerPxl;
			} else {
				renderError = 'samplesPerPxl';
				render = false;
			}
			if (data.window !== undefined) {
				switch (data.window) {
					case 1:
						this.wFunction = this.myWindow.BARTLETT;
						break;
					case 2:
						this.wFunction = this.myWindow.BARTLETTHANN;
						break;
					case 3:
						this.wFunction =this.myWindow.BLACKMAN;
						break;
					case 4:
						this.wFunction = this.myWindow.COSINE;
						break;
					case 5:
						this.wFunction = this.myWindow.GAUSS;
						break;
					case 6:
						this.wFunction = this.myWindow.HAMMING;
						break;
					case 7:
						this.wFunction = this.myWindow.HANN;
						break;
					case 8:
						this.wFunction = this.myWindow.LANCZOS;
						break;
					case 9:
						this.wFunction = this.myWindow.RECTANGULAR;
						break;
					case 10:
						this.wFunction = this.myWindow.TRIANGULAR;
						break;
				}
			} else {
				renderError = 'window';
				render = false;
			}
			if (data.imgWidth !== undefined) {
				this.imgWidth = data.imgWidth;
			} else {
				renderError = 'imgWidth';
				render = false;
			}
			if (data.imgHeight !== undefined) {
				this.imgHeight = data.imgHeight;
			} else {
				renderError = 'imgHeight';
				render = false;
			}
			if (data.dynRangeInDB !== undefined) {
				this.dynRangeInDB = data.dynRangeInDB;
			} else {
				renderError = 'dynRangeInDB';
				render = false;
			}
			if (data.pixelRatio !== undefined) {
				this.pixelRatio = data.pixelRatio;
			} else {
				renderError = 'pixelRatio';
				render = false;
			}
			if (data.sampleRate !== undefined) {
				this.sampleRate = data.sampleRate;
			} else {
				renderError = 'sampleRate';
				render = false;
			}
			if (data.audioBufferChannels !== undefined) {
				this.audioBufferChannels = data.audioBufferChannels;
			} else {
				renderError = 'audioBufferChannels';
				render = false;
			}
			if (data.transparency !== undefined) {
				this.transparency = data.transparency;
			} else {
				renderError = 'transparency';
				render = false;
			}
			if (data.audioBuffer !== undefined) {
				this.audioBuffer = data.audioBuffer;
			} else {
				renderError = 'audioBuffer';
				render = false;
			}
			if (data.drawHeatMapColors !== undefined) {
				this.drawHeatMapColors = data.drawHeatMapColors;
			} else {
				renderError = 'drawHeatMapColors';
				render = false;
			}
			if (data.preEmphasisFilterFactor !== undefined) {
				this.preEmphasisFilterFactor = data.preEmphasisFilterFactor;
			} else {
				renderError = 'preEmphasisFilterFactor';
				render = false;
			}
			if (data.heatMapColorAnchors !== undefined) {
				console.log(data.heatMapColorAnchors[0]);
				this.heatMapColorAnchors = [
					[data.heatMapColorAnchors.slice(0, 3)],
					[data.heatMapColorAnchors.slice(3, 6)],
					[data.heatMapColorAnchors.slice(6, 9)],
				];
			} else {
				renderError = 'heatMapColorAnchors';
				render = false;
			}
			if (data.invert !== undefined) {
				this.invert = data.invert;
			} else {
				renderError = 'invert';
				render = false;
			}
			if (render) {
				return(this._renderSpectrogram());
			} else {
				throw new Error(renderError + ' is undefined');
			}
		} else {
			throw new Error("data object has to be passed in to renderSpectrogram function");
		}
	};
	//
	/////////////////////
}