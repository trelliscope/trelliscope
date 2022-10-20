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
} from './meta_types';

import {
  Meta,
  NumberMeta,
  StringMeta,
  FactorMeta,
} from './meta_classes';

const data = [
  { var1: 3, var2: 'a' },
  { var1: 1, var2: 'b' },
  { var1: 4, var2: 'c' },
];

console.log(data);

const var1 = new NumberMeta({ varname: 'var1' });
console.log('var1 new', var1);

console.log(var1.validateName(data))

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
