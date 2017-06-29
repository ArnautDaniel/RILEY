$(function removeitem(item) {
   
    var form  = $("[id='" + item + "']");
    $(form).submit(function(event) {
	event.preventDefault();
	var desc = $("[id='" + item + "']").find("#item");
	var price = $("[id='" + item + "']").find("#item-price");
	var qty = $("[id='" + item + "']").find("#item-quantity");
	$.ajax({
	    type: 'POST',
	    url: 'removeitem',
	    data: {
		'item': desc,
		'item-price': price,
		'item-quantity': qty
	    },
	    success: function() {
		form.remove();
	    },
	    error: function() {
		Materialize.toast('Error removing', 3000);
	    }
	});
    });
});
