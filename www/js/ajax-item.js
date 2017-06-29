$(function additem() {
    var form = $('#new-item');
    $(form).submit(function(event) {
        event.preventDefault();
        var desc = document.getElementById('input-item-description').value;
        var price = document.getElementById('input-item-price').value;
        var qty = document.getElementById('input-item-qty').value;
        var imageloc = document.getElementById('input-picture');

        $.ajax({
            type: 'POST',
            url: 'additem',
            data: {
                'input-item-qty': qty,
                'input-item-description': desc,
                'input-item-price': price,
                'image-data': imageloc.name
            },
            success: function() {
 
                var newimg = $("#image-name");

                if (document.getElementById('saveform') == null) {
		     Materialize.toast('Added ' + desc + ' to invoice', 4000);
                    $("#input-picture").replaceWith("<p class='center'>'No pictures available'</p>");
		    $("#write-up-box").fadeOut(0);
		    $("#write-up-box").attr("class", "box collapsed-box")
		    $("#write-up-box").fadeIn("slow");
		    var source = $('#new-pic-template').html();
		    var template = Handlebars.compile(source);
		    var context = {imgname: imageloc.name , caption: desc , price: price , quantity: qty};
		    var newpic = template(context);
		    $("#myUL").append(newpic);
		    $('.carousel').carousel();
		    $('.materialboxed').materialbox();
                } else {
		    var source = $('#new-pic-template').html();
		    var template = Handlebars.compile(source);
		    var context = {imgname: imageloc.name , caption: desc , price: price , quantity: qty};
		    var newpic = template(context);
                    $("#input-picture").replaceWith("<img id='input-picture' src='" + $("#image-name").attr("value") + "' class='img-responsive' width='40%' height='40%' name='" + $("#image-name").attr("value") + "' />");
		    Materialize.toast('Added ' + desc + ' to invoice', 4000);
                    $("#input-picture").fadeOut(0);
                    $("#input-picture").fadeIn("slow");
                    document.getElementById('saveform').remove();
		    document.getElementById('image-name').remove();
		    $("#myUL").append(newpic);
		    $('.carousel').carousel();
		    $('.materialboxed').materialbox();
                }
                $("#new-item").trigger("reset");
		$("html").animate({scrollTop: 0}, "slow");
                $("#input-item-description").focus();
		

            },
            error: function() {
                alert('error');
            }
        });
    });
});

$(function emptypics() {
    if ($("#input-picture").attr("src") == null) {
        $("#input-picture").replaceWith("<p> Add more pictures to continue </p>");
	$("#write-up-box").attr("class", "box collapsed-box");
    }
    
});
