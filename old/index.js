const R = require('r-script');

const r =R('./interface/test.R');
console.log(r);
  r.data({ test: 'hello' })
  .call(function(err, d) {
    console.log(err);
    if (err) throw err;
    console.log(d);
  });