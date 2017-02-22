$(function() { var form = $('#new-item');
var formMessages = $('#form-messages');
$(form).submit(function(event) { event.preventDefault();
var desc = document.getElementById('input-item-description').value;
var price = document.getElementById('input-item-price').value;
var qty = document.getElementById('input-item-qty').value;
var imageloc = document.getElementById('input-picture');

$.ajax({
type: 'POST',
url: 'additem',
data: {
'input-item-qty' : qty ,
'input-item-description' : desc ,
'input-item-price' : price ,
'image-data' : imageloc.name },
success: function () {
var table = document.getElementById('itemlist');
var row = table.insertRow(1);
var cell1 = row.insertCell(0);
var cell2 = row.insertCell(1);
var cell3 = row.insertCell(2);
var cell4 = row.insertCell(3);
cell1.innerHTML = desc;
cell2.innerHTML = price;
cell3.innerHTML = qty;
var newimg = $("#saveform");
jQuery("#saveform").detach().appendTo("#box-picture");
jQuery("#input-picture").detach()

},
error: function() {
alert('error');
} } ); }); });

$("#myparentID > img").first();
