import {
  MetaType,
  IMeta,
  INumberMeta,
  IStringMeta,
  IFactorMeta,
  IDateMeta,
  IDatetimeMeta,
  IHrefMeta,
  IGeoMeta,
  IGraphMeta,
  GraphDirection,
  IDisplay,
} from './types';

import {
  Meta,
  NumberMeta,
  StringMeta,
  FactorMeta,
  Display,
} from './classes';

const data = [
  { var1: 3, var2: 'a' },
  { var1: 1, var2: 'b' },
  { var1: 4, var2: 'c' },
];

console.log(data);

const var1 = new NumberMeta({ varname: 'var1' });
console.log('var1 new', var1);

// console.log(var1.validateName(data))

const var1_1 = new NumberMeta({
  varname: 'var1',
  locale: false
});
console.log('var1 new with locale', var1_1);

const var1Json = {
  "varname": "var1",
  "type": "number",
  "tags": ["a", "b", "c"],
  "filterable": true,
  "sortable": true,
  "label": "test",
  "locale": false
};

let newVar1_1 = var1Json as INumberMeta;
console.log('var1 as interface', newVar1_1);

let newVar1_2 = var1Json as NumberMeta;
console.log('var1 as class', newVar1_2);

const var2 = new StringMeta({ varname: 'var2' });
console.log(var2);

const var2Json = {
  "varname": "var2",
  "type": "string",
  "tags": "common",
  "filterable": true,
  "sortable": true,
  "label": "test"  
};

const var2fJson = {
  "varname": "var2",
  "type": "factor",
  "tags": ["a", "b", "c"],
  "filterable": true,
  "sortable": true,
  "label": "test",
  "levels": ["a", "b", "c"]
};

let newVarf2 = var2fJson as FactorMeta;
console.log('newVarf2', newVarf2);


const displ = {
  "name": "Highway mpg vs. City mpg",
  "description": "Highway mpg vs. City mpg",
  "key_cols": ["manufacturer", "class"],
  "metas": [
    {
      "sortable": true,
      "filterable": true,
      "tags": [],
      "label": "Vehicle manufacturer name",
      "type": "string",
      "varname": "manufacturer"
    },
    {
      "levels": ["subcompact", "compact", "2seater", "midsize", "suv", "minivan", "pickup"],
      "sortable": true,
      "filterable": true,
      "tags": [],
      "label": "Type of vehicle",
      "type": "factor",
      "varname": "class"
    },
    {
      "locale": true,
      "digits": null,
      "sortable": true,
      "filterable": true,
      "tags": "metrics",
      "label": "Mean city miles per gallon",
      "type": "number",
      "varname": "mean_cty"
    }
  ],
  "state": {
    "layout": {
      "page": 1,
      "arrange": "rows",
      "ncol": 3,
      "nrow": 2,
      "type": "layout"
    },
    "labels": {
      "varnames": ["manufacturer", "class"],
      "type": "labels"
    },
    "sort": [
      {
        "dir": "asc",
        "varname": "class",
        "type": "sort"
      },
      {
        "dir": "asc",
        "varname": "mean_cty",
        "type": "sort"
      }
    ],
    "filter": [
      {
        "values": ["audi", "volkswagen"],
        "regexp": null,
        "filtertype": "category",
        "varname": "manufacturer",
        "type": "filter"
      },
      {
        "max": null,
        "min": 20,
        "filtertype": "numberrange",
        "varname": "mean_cty",
        "type": "filter"
      }
    ]
  },
  "views": [
    {
      "name": "test view",
      "state": {
        "layout": {
          "page": 1,
          "arrange": "rows",
          "ncol": 5,
          "nrow": 3,
          "type": "layout"
        },
        "labels": {
          "varnames": ["class", "manufacturer"],
          "type": "labels"
        },
        "sort": [
          {
            "dir": "asc",
            "varname": "manufacturer",
            "type": "sort"
          },
          {
            "dir": "desc",
            "varname": "mean_cty",
            "type": "sort"
          }
        ],
        "filter": [
          {
            "values": ["toyota", "honda", "nissan", "subaru"],
            "regexp": null,
            "filtertype": "category",
            "varname": "manufacturer",
            "type": "filter"
          },
          {
            "max": 20,
            "min": null,
            "filtertype": "numberrange",
            "varname": "mean_cty",
            "type": "filter"
          }
        ]
      }
    },
    {
      "name": "test view 2",
      "state": {
        "layout": {
          "page": 1,
          "arrange": "rows",
          "ncol": 3,
          "nrow": 2,
          "type": "layout"
        },
        "labels": {
          "varnames": ["manufacturer", "class"],
          "type": "labels"
        },
        "sort": [
          {
            "dir": "asc",
            "varname": "class",
            "type": "sort"
          }
        ],
        "filter": [
          {
            "max": 25,
            "min": 15,
            "filtertype": "numberrange",
            "varname": "mean_cty",
            "type": "filter"
          }
        ]
      }
    }
  ],
  "inputs": [
    {
      "options": ["no", "yes"],
      "type": "radio",
      "active": true,
      "label": "Is it good?",
      "name": "good_radio"
    },
    {
      "options": ["no", "yes"],
      "type": "checkbox",
      "active": true,
      "label": "Is it good?",
      "name": "good_checkbox"
    },
    {
      "options": ["no", "yes"],
      "type": "select",
      "active": true,
      "label": "Is it good?",
      "name": "good_select"
    },
    {
      "options": ["no", "yes"],
      "type": "multiselect",
      "active": true,
      "label": "Is it good?",
      "name": "good_multiselect"
    },
    {
      "height": 6,
      "width": 100,
      "type": "text",
      "active": true,
      "label": "What do you think?",
      "name": "opinion"
    },
    {
      "type": "number",
      "active": true,
      "label": "Rank this panel",
      "name": "rank"
    }
  ]
};
