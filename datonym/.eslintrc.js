module.exports = {
  root: true,
  env: {
    node: true,
  },
  'extends': [
    'plugin:vue/essential',
    '@vue/airbnb',
  ],
  rules: {
    'linebreak-style': 'off',
    'indent': 'off',
    'no-alert': 'off', // TODO re-enable
    'brace-style': ['error', 'stroustrup'],
    'vue/script-indent': ['error', 2, {
      'baseIndent': 1,
    }],
    'keyword-spacing': ['error', {
      'before': true,
      'after': true,
      'overrides': {
        'if': {
          'after': false,
        },
      }
    }],
    'no-console': process.env.NODE_ENV === 'production' ? 'error' : 'off',
    'no-debugger': process.env.NODE_ENV === 'production' ? 'error' : 'off',
  },
  parserOptions: {
    parser: 'babel-eslint',
  }
}