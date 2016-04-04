# raf.js [![Version](http://img.shields.io/badge/version-0.0.4-brightgreen.svg)](https://github.com/ngryman/jquery.finger#release-history) [![Size](http://img.shields.io/badge/size-0.25%20kB-blue.svg)](https://raw2.github.com/ngryman/raf.js/master/raf.min.js)

Yet another `requestAnimationFrame` polyfill:
- really tiny: **361 bytes minified** and **242 gzipped**.

## Install

|Bower|Jam|npm|
|-----|---|---|
|`bower install raf.js`|`jam install raf`|`npm install raf.js`|

## Release History

```
v0.0.4
 - IE8 compatibility, (#6).

v0.0.3
 - removed unecessary cast on `Date.now` (#1).
 - export to `cancelAnimationFrame` instead of `cancelRequestAnimationFrame` (#2).
```

## Credits

Paul's Irish [gist](https://gist.github.com/paulirish/1579671).
