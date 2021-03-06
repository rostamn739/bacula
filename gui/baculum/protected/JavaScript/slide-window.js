var SlideWindowClass = Class.create({

	windowId: null,
	window: null,
	showEl: null,
	hideEl: null,
	fullSizeEl : null,
	search: null,
	toolbar: null,
	tools: null,
	titlebar: null,
	options: null,
	configurationObj: null,
	loadRequest : null,
	actionsRequest: null,
	repeaterEl: null,
	gridEl: null,
	checked: [],
	objects: {},
	windowSize: null,
	initElementId: null,

	size: {
		widthNormal : '53%',
		heightNormal : '325px',
		widthHalf : '53%',
		heightHalf : '586px',
		widthFull : '100%',
		heightFull : '586px',
		menuWidth: '75px'
	},

	elements : {
		content: 'div.slide-window-content',
		containerSuffix: '-slide-window-container',
		containerProgressSuffix: '-slide-window-progress',
		configurationWindows: 'div.configuration',
		configurationProgress: 'div.configuration-progress',
		contentItems: 'slide-window-element',
		contentAlternatingItems: 'slide-window-element-alternating',
		toolsButtonSuffix: '-slide-window-tools',
		optionsButtonSuffix: '-slide-window-options',
		actionsSuffix: '-slide-window-actions',
		toolbarSuffix: '-slide-window-toolbar',
		titleSuffix: '-slide-window-title',
		actionsButton : 'actions_btn'
	},

	initialize: function(windowId, data) {
		if(typeof(windowId) == "undefined") {
			return;
		}

		this.windowId = windowId;
		this.window = $(this.windowId + this.elements.containerSuffix);
		this.tools = $(this.windowId + this.elements.toolsButtonSuffix);
		this.options = $(this.windowId + this.elements.optionsButtonSuffix);
		this.titlebar = $(this.windowId + this.elements.titleSuffix);

		if(data.hasOwnProperty('showId')) {
				this.showEl = $(data.showId);
		} else {
			alert('slide-window.js - "showId" property does not exists.');
			return false;
		}

		if(data.hasOwnProperty('hideId')) {
			this.hideEl = $(data.hideId);
		} else {
			alert('slide-window.js - "hideId" property does not exists.');
			return false;
		}

		if(data.hasOwnProperty('fullSizeId')) {
			this.fullSizeEl = $(data.fullSizeId);
		} else {
			alert('slide-window.js - "fullSizeId" property does not exists.');
			return false;
		}

		if(data.hasOwnProperty('search')) {
			this.search = $(data.search);
		} else {
			alert('slide-window.js - "search" property does not exists.');
			return false;
		}
		this.setEvents();
	},

	objectExists: function(key) {
		return this.objects.hasOwnProperty(key);
	},

	registerObj: function(key, obj) {
		if(this.objectExists(key) === false) {
			this.objects[key] = obj;
		}
	},

	getObj: function(key) {
		var obj = null;
		if(this.objectExists(key) === true) {
			obj = this.objects[key];
		}
		return obj;
	},

	setEvents: function() {
		this.showEl.observe('click', function(){
			this.openWindow();
		}.bind(this));

		this.hideEl.observe('click', function(){
			this.closeWindow();
		}.bind(this));
		
		this.fullSizeEl.observe('click', function(){
			this.resetSize();
		}.bind(this));

		this.titlebar.observe('dblclick', function() {
			this.resetSize();
		}.bind(this));

		this.search.observe('keyup', function(){
			this.setSearch();
		}.bind(this));

		this.tools.observe('click', function() {
			this.toggleToolbar();
		}.bind(this));

		this.options.observe('click', function() {
			this.toggleToolbar();
		}.bind(this));

		this.setActionsBtnEvents();
	},

	setActionsBtnEvents: function() {
		var actions_btn_container = this.window.getElementsByClassName(this.elements.actionsButton);
		if (actions_btn_container.length === 1) {
			var actions_btn = actions_btn_container[0].getElementsByTagName('INPUT');
			if (actions_btn.length === 1) {
				actions_btn[0].addEventListener('mouseup', function(e) {
					var row = this.getGridRowUnderCursor(e);
					var el = $(row).down('input[type=hidden]');
					if(el) {
						var val = el.getValue();
						this.actionsRequest.ActiveControl.CallbackParameter = val;
						this.actionsRequest.dispatch();
					}
				}.bind(this));
			}
		}
	},

	openWindow : function() {
		this.hideOtherWindows();
		Effect.toggle(this.window, 'slide', { duration: 0.1, afterFinish : function() {
				this.halfSizeWindow();
			}.bind(this)
		});
	},

	closeWindow : function() {
		Effect.toggle(this.window, 'slide', { duration: 0.3, afterFinish : function() {
				this.resetSize();
			}.bind(this)
		});
	},

	isWindowOpen: function() {
		return !(this.window.style.display === 'none');
	},

	showProgress: function(show) {
		var progress = $(this.windowId + this.elements.containerProgressSuffix);
		if (show === true) {
			progress.setStyle({display: 'block'});
		} else if (show === false) {
			progress.hide();
		}
	},

	resetSize : function() {
		if(this.isConfigurationOpen()) {
			if(this.isFullSize()) {
				this.halfSizeWindow();
			} else if(this.isHalfSize()) {
					this.normalSizeWindow();
			} else if (this.isNormalSize()){
				this.halfSizeWindow();
			} else {
				this.normalSizeWindow();
			}
		} else {
			if(this.isFullSize()) {
				this.normalSizeWindow();
			} else if(this.isHalfSize() || this.isNormalSize()) {
				this.fullSizeWindow();
			}
		}
	},

	isNormalSize: function() {
		return (this.windowSize == this.size.widthNormal && this.window.getHeight()  + 'px' == this.size.heightNormal);
	},

	isHalfSize: function() {
		return (this.windowSize == this.size.widthHalf && this.window.getHeight()  + 'px' == this.size.heightHalf);
	},

	isFullSize: function() {
		return (this.windowSize  == this.size.widthFull && this.window.getHeight()  + 'px' == this.size.heightFull);
	},

	normalSizeWindow: function() {
			new Effect.Morph(this.window, {style : 'width: ' + this.size.widthNormal + '; height: ' + this.size.heightNormal + ';', duration : 0.4});
			this.windowSize = this.size.widthNormal;
	},
	
	halfSizeWindow: function() {
			new Effect.Morph(this.window, {style : 'width: ' + this.size.widthHalf + '; height: ' + this.size.heightHalf + ';', duration : 0.4});
			this.windowSize = this.size.widthHalf;
	},
	
	fullSizeWindow: function() {
			new Effect.Morph(this.window, {style : 'width: ' + this.size.widthFull + '; height: ' + this.size.heightFull + ';', duration : 0.4});
			this.windowSize = this.size.widthFull;
	},

	hideOtherWindows: function() {
		$$('.slide-window-container').each(function(el, index) {
			el.setStyle({
				display : 'none',
				width : this.size.widthNormal,
				height : this.size.heightNormal
			});
		}.bind(this));
	},

	setConfigurationObj: function(obj) {
		this.configurationObj = obj;
	},

	setWindowElementsEvent: function(opts) {
		this.repeaterEl = opts.repeater_id + '_Container';
		this.gridEl = opts.grid_id;
		this.loadRequest = opts.request_obj;
		if (opts.hasOwnProperty('actions_obj')) {
			this.actionsRequest = opts.actions_obj;
		}

		if (this.initElementId) {
			this.openConfigurationById(this.initElementId);
			this.initElementId = null;
			// for open window by init element, default set second tab
			this.configurationObj.switchTabByNo(2);
		}

		this.showProgress(false);
		this.markAllChecked(false);
		this.setLoadRequest();
		this.postWindowOpen();
	},

	setLoadRequest: function() {
		var dataList = [];
		var repeater = $(this.repeaterEl);
		var grid = $(this.gridEl);
		if(grid) {
			dataList = grid.select('tr');
			this.makeSortable();
		} else if (repeater) {
			dataList = repeater.select('div.slide-window-element');
		}

		var set_callback_parameter = function(element) {
			var el = $(element).down('input[type=hidden]')
			if(el) {
				var val = el.getValue();
				this.openConfigurationById(val);
			}
		}.bind(this);
		this.setSearch();
		dataList.each(function(tr) {
			$(tr).observe('click', function(index, clickedEl) {
				var target = clickedEl.target || clickedEl.srcElement;
				var clicked = $(target.id);
				// for element selection action (clicked checkbox) configuration window is not open
				if(clicked && clicked.hasAttribute('type') && clicked.readAttribute('type') == 'checkbox') {
					return;
				}
				set_callback_parameter(tr);
			}.bind(this, tr));
		}.bind(this));
		Formatters.set_formatters();
		this.revertSortingFromCookie();
	},

	openConfigurationById: function(id) {
		this.loadRequest.ActiveControl.CallbackParameter = id;
		this.loadRequest.dispatch();
		this.configurationObj.openConfigurationWindow(this);
        },

	isConfigurationOpen: function() {
		var is_open = false;
		$$(this.elements.configurationWindows, this.elements.configurationProgress).each(function(el) {
			if(el.getStyle('display') == 'block') {
				is_open = true;
				throw $break;
			}
		}.bind(is_open));
		return is_open;
	},

	sortTable: function (grid_id, col, reverse, set_cookie) {
		var table = document.getElementById(grid_id);
		var tb = table.tBodies[0], tr = Array.prototype.slice.call(tb.rows, 0), i;
		reverse = -((+reverse) || -1);
		tr = tr.sort(function (a, b) {
			var val, val_a, val_b, el_a, el_b;
			el_a = a.cells[col].childNodes[1];
			el_b = b.cells[col].childNodes[1];
			if (el_a && el_b && el_a.nodeType === 1 && el_b.nodeType === 1 && el_a.hasAttribute('rel') && el_b.hasAttribute('rel')) {
				val_a = el_a.getAttribute('rel');
				val_b = el_b.getAttribute('rel');
			} else {
				val_a = a.cells[col].textContent.trim();
				val_b = b.cells[col].textContent.trim();
			}
			if (!isNaN(parseFloat(val_a)) && isFinite(val_a) && !isNaN(parseFloat(val_b)) && isFinite(val_b)) {
				val = val_a - val_b
			} else {
				val = val_a.localeCompare(val_b);
			}
			return reverse * (val);
		});
		var even;
		for (i = 0; i < tr.length; i++) {
			even = ((i % 2) == 0);
			if (even) {
				tr[i].className = this.elements.contentItems;
			} else {
				tr[i].className = this.elements.contentAlternatingItems;
			}
			tb.appendChild(tr[i]);
		}
		if (set_cookie === true) {
			Cookies.set_cookie(this.gridEl, col + ':' + reverse);
		}
	},

	makeSortable: function (grid) {
		var self = this;
		var grid_id, set_cookie;
		if (grid) {
			grid_id = grid;
			// for external grids (non-slide) do not remember sorting order
			set_cookie = false;
		} else {
			grid_id = this.gridEl;
			set_cookie = true;
		}
		var table = document.getElementById(grid_id);
		table.tHead.style.cursor = 'pointer';
		var th = table.tHead, i;
		th && (th = th.rows[0]) && (th = th.cells);
		if (th) {
			i = th.length;
		} else {
			return;
		}
		var downCounter = 0;
		// skip first column if in column header is input (checkbox for elements mark)
		if (th[0].childNodes[0].nodeName == "INPUT") {
			downCounter = 1;
		}
		while (--i >= downCounter) (function (i) {
			var dir = 1;
			th[i].addEventListener('click', function () {
				self.sortTable(grid_id, i, (dir = 1 - dir), set_cookie);
			});
		}(i));
	},

	revertSortingFromCookie: function() {
		var sorting = Cookies.get_cookie(this.gridEl);
		if (sorting != null) {
			var sort_param = sorting.split(':');
			var col = parseInt(sort_param[0], 10);
			var order = -(parseInt(sort_param[1], 10));
			this.sortTable(this.gridEl, col, order);
		}
	},

	setSearch: function() {
		var search_pattern = new RegExp(this.search.value, 'i');
		var repeater = $(this.repeaterEl);
		var grid = $(this.gridEl);
		if (repeater) {
			repeater.select('div.' + this.elements.contentItems).each(function(value){
				if(search_pattern.test(value.childNodes[2].textContent) == false) {
					value.setStyle({'display' : 'none'});
				} else {
					value.setStyle({'display' : ''});
				}
			}.bind(search_pattern));
		}

		if (grid) {
			grid.select('tbody tr').each(function(value) {
				var tds = value.select('td');
				var td;
				var found = false;
				for (var i = 0; i < tds.length; i++) {
					td = tds[i].textContent.trim();
					if(search_pattern.test(td) == true) {
						found = true;
						break;
					}
				}

				if(found === true) {
					value.show();
				} else {
					value.hide();
				}
			}.bind(search_pattern));
		}
	},
	setElementsCount : function() {
		var elements_count = $$('div[id="' + this.windowId + this.elements.containerSuffix + '"] div.' + this.elements.contentItems).length || $$('div[id="' + this.windowId + this.elements.containerSuffix + '"] tr.' + this.elements.contentItems + ', div[id="' + this.windowId + this.elements.containerSuffix + '"] tr.' + this.elements.contentAlternatingItems).length;
		var count_el = $(this.windowId + this.elements.titleSuffix).getElementsByTagName('span')[0];
		$(count_el).update(' (' + elements_count + ')');
	},
	toggleToolbar: function() {
		if (this.isToolbarOpen() === false) {
			this.markAllChecked(false);
		}
		Effect.toggle($(this.windowId + this.elements.toolbarSuffix), 'slide', { duration: 0.2});
	},
	isToolbarOpen: function() {
		return $(this.windowId + this.elements.toolbarSuffix).visible();
	},
	setActions: function() {
		var checkboxes = this.getCheckboxes();
		checkboxes.each(function(el) {
			el.observe('change', function() {
				var is_checked = this.isAnyChecked(checkboxes);
				if(is_checked === true && !this.areActionsOpen()) {
					this.showActions();
				} else if (is_checked === false && this.areActionsOpen()) {
					this.hideActions();
				}
			}.bind(this));
                }.bind(this));
	},
	isAnyChecked: function(checkboxes) {
		var is_checked = false;
		checkboxes.each(function(ch) {
			if(ch.checked == true) {
				is_checked = true;
				throw $break;
			}
		});
		return is_checked;
	},

	getCheckboxes: function() {
		var grid = $(this.gridEl);
		var checkboxes = [];
		if (grid) {
			checkboxes = grid.select('input[name="actions_checkbox"]');
		}
		return checkboxes;
	},

	areCheckboxesChecked: function() {
		var checkboxes = this.getCheckboxes();
		return this.isAnyChecked(checkboxes);
	},

	markAllChecked: function(check) {
		this.checked = [];
		var checkboxes = this.getCheckboxes();
		var containerId;
		if(checkboxes.length > 0) {
			checkboxes.each(function(ch, index) {
				if (ch.up('tr').visible()) {
					containerId = ch.getAttribute('rel');
					if (ch.checked == false && check == true) {
						ch.checked = true;
					} else if (ch.checked == true && check == false) {
						ch.checked = false;
					}
					this.markChecked(containerId, ch.checked, ch.value);
				}
			}.bind(this));
			if (containerId) {
				this.packChecked(containerId);
			}
		}

		if(check) {
			this.showActions();
		} else {
			this.hideActions();
		}
	},
	markChecked: function(containerId, checked, param, pack) {
		if (this.checked.length == 0) {
			if(checked == true) {
				this.checked.push(param);
			}
		} else {
			index = this.checked.indexOf(param);
			if(checked === true && index == -1) {
				this.checked.push(param);
			} else if (checked === false && index > -1) {
				this.checked.splice(index, 1);
			}
		}

		if(checked == true) {
			this.showActions();
		} else if(this.checked.length == 0) {
			this.hideActions();
		}

		if (pack === true) {
			this.packChecked(containerId);
		}
	},
	packChecked: function(containerId) {
		var values_packed = this.checked.join(';');
		$(containerId).setValue(values_packed);
	},
	showActions: function() {
		if (this.areActionsOpen()) {
			return;
		}
		if (this.isToolbarOpen()) {
			this.toggleToolbar();
		}
		Effect.toggle($(this.windowId + this.elements.actionsSuffix), 'slide', { duration: 0.2});
	},
	hideActions: function() {
		if (!this.areActionsOpen()) {
			return;
		}
		this.checked = [];
		Effect.toggle($(this.windowId + this.elements.actionsSuffix), 'slide', { duration: 0.2});
	},
	areActionsOpen: function() {
		return $(this.windowId + this.elements.actionsSuffix).visible();
	},
	postWindowOpen: function() {
		this.setActions();
		this.setElementsCount();
		this.setOptionsBtn();
	},
	setOptionsBtn: function() {
		var options_btn = this.window.getElementsByClassName(this.elements.actionsButton);
		var table_window = $(this.windowId + this.elements.containerSuffix).down(this.elements.content);
		if (options_btn.length === 1) {
			options_btn = options_btn[0];
			table_window.stopObserving('mouseover');
			table_window.observe('mouseover', function(e) {
				var el = this.getGridRowUnderCursor(e);
				if (el && (el.className == this.elements.contentItems || el.className == this.elements.contentAlternatingItems)) {
					el.style.backgroundColor = '#aeb2b6';
					options_btn.setStyle({display: ''});
					var scroll_y = document.viewport.getScrollOffsets().top;
					var y = (el.viewportOffset().top + scroll_y - 57).toString() + 'px';
					options_btn.setStyle({top: y});
				} else {
					options_btn.setStyle({display: 'none'});
				}
			}.bind(this));
			table_window.stopObserving('mouseout');
			table_window.observe('mouseout', function(e) {
				table_window.select('TR').forEach(function(el) {
					el.style.backgroundColor = '';
				});;
				options_btn.setStyle({display: 'none'});
			});
		}
	},
	getGridRowUnderCursor: function(e) {
		var x = e.clientX - 100;
		var y = e.clientY;
		var element_mouse_is_over = document.elementFromPoint(x, y);
		var el;
		var el_over = $(element_mouse_is_over);
		if (el_over && el_over.nodeName != 'TR') {
			el = el_over.down('tr');
			if (!el) {
				el = el_over.up('tr');
			}
		}
		return el;
	},
	setInitElementId: function(id) {
		this.initElementId = id;
	},
	quickJumpToElement: function(id, btn_id, panel_obj) {
		this.setInitElementId(id);
		panel_obj.show('container');
		if (this.isWindowOpen() === true) {
			this.openConfigurationById(id);
		} else {
			$(btn_id).click();
		}
	}
});

var SlideWindow = new SlideWindowClass()

document.observe("dom:loaded", function() {
	if(Prototype.Browser.IE  || Prototype.Browser.Gecko || Prototype.Browser.WebKit) {
		$$('input[type=checkbox], input[type=submit], input[type=radio], input[type=image], a').each(function(el) {
			el.observe('focus', function() {
				el.blur();
			}.bind(el));
		});
	}
});

function setContentWidth() {
	var content_width = $('container').getWidth() - $('menu-left').getWidth() - 1;
	$('content').setStyle({'width': content_width + 'px'});
}


Event.observe(window, 'resize', function() {
	setContentWidth();
});

document.observe("dom:loaded", function() {
	setContentWidth();
});
