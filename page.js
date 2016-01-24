$(document).ready(function() {
	var sourceSuggestions = function(id, section) {
		return function(q, cb) {
			if (!typeaheadUpdatedSuggestions[id]) {
				return;
			}
			var suggestions = [];
			$.each(typeaheadUpdatedSuggestions[id][section], function(i, entry) {
				suggestions.push({value: typeaheadUpdatedSuggestions[id]['phrase'] + ' ' + entry['suggestion'],
					phrase: typeaheadUpdatedSuggestions[id]['phrase'],
					suggestion: entry['suggestion'],
					score: entry['score']});
			});
			cb(suggestions);
		};
	};

	$('#q').typeahead(null,
		{name: 'section-1-id',
			source: sourceSuggestions('q', 'section-1-id'),
			templates: {
				header: "<h3 class='section-name'>Section 1</h3>",
				suggestion: function(data) {
					return '<p>' + data.phrase + ' <strong>' + data.suggestion + '</strong> &mdash; ' + data.score + '</p>';
				}}},
		{name: 'section-2-id',
			source: sourceSuggestions('q', 'section-2-id'),
			templates: {
				header: "<h3 class='section-name'>Section 2</h3>",
				suggestion: function(data) {
					return '<p>' + data.phrase + ' <strong>' + data.suggestion + '</strong> &mdash; ' + data.score + '</p>';
				}}},
		{name: 'section-3-id',
			source: sourceSuggestions('q', 'section-3-id'),
			templates: {
				header: "<h3 class='section-name'>Section 3</h3>",
				suggestion: function(data) {
					return '<p>' + data.phrase + ' <strong>' + data.suggestion + '</strong> &mdash; ' + data.score + '</p>';
				}}});
});
