import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  input: 'pong.js',
  output: {
    file: 'pong-bundle.js',
    format: 'iife'
  },
  plugins: [
    nodeResolve({
      jsnext: true,
      main: true
    }),
    commonjs()
  ]
};
