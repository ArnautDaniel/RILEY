
var options = [
    {selector: '#myUL', offset: 0, callback: function(el) {
	Materialize.fadeInImage($(el)); } } ];
Materialize.scrollFire(options);

