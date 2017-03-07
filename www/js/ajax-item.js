$(function() {
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
                var table = document.getElementById('itemlist');
                var row = table.insertRow(1);
                var cell1 = row.insertCell(0);
                var cell2 = row.insertCell(1);
                var cell3 = row.insertCell(2);
                var cell4 = row.insertCell(3);
                cell1.innerHTML = desc;
                cell2.innerHTML = price;
                cell3.innerHTML = qty;
                var newimg = $("#image-name");

                if ($("#image-name").attr("value") == null) {
                    $("#input-picture").replaceWith("<p> All out of pictures </p>");
		    $("#write-up-box").fadeOut(0);
		    $("#write-up-box").attr("class", "box collapsed-box")
		    $("#write-up-box").fadeIn("slow");
                } else {
                    $("#input-picture").replaceWith("<img id='input-picture' src='" + $("#image-name").attr("value") + "' class='img-responsive' width='40%' height='40%' name='" + $("#image-name").attr("value") + "' />");
                    $("#input-picture").fadeOut(0);
                    $("#input-picture").fadeIn("slow");
                    $('#modalImg').find('form').first().remove();
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

$(function () {
    if ($("#input-picture").attr("src") == null) {
        $("#input-picture").replaceWith("<p> Add more pictures to continue </p>");
	$("#write-up-box").attr("class", "box collapsed-box");
    }
    
});
