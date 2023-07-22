// generate json to compare with what is generated from R
import { writeFileSync } from 'fs';

// sort the output objects by key so we can ensure we are
// checking the JSON generated by R in the same order
function sortKeys(obj: any) {
  const res = Object.keys(obj)
    .sort()
    .reduce(
      (acc, key) => ({
        ...acc,
        [key]: obj[key],
      }),
      {}
    );
  return res;
}

function writeCase(obj: any, name: string) {
  writeFileSync(`${prefix}/${name}.json`, JSON.stringify(sortKeys(obj)));
}

import {
  NumberMeta,
  StringMeta,
  FactorMeta,
  DateMeta,
  DatetimeMeta,
  HrefMeta,
  GeoMeta,
  GraphMeta,
  RadioInput,
  CheckboxInput,
  SelectInput,
  MultiselectInput,
  TextInput,
  NumberInput,
  LayoutState,
  LabelState,
  SortState,
  CategoryFilterState,
  NumberRangeFilterState,
  DateRangeFilterState,
  DatetimeRangeFilterState,
  DisplayState,
  View,
  Display,
  Inputs,
  InputClientSideStorage,
  InputEmailFeedback,
  PanelSource,
  PanelMeta,
} from './classes';

// const prefix = 'inst/types/json'
const prefix = 'json';

/* ------------------------------------------------------ */
/* meta                                                   */
/* ------------------------------------------------------ */

const meta_num_min = new NumberMeta({ varname: 'numvar' });
writeCase(meta_num_min, 'meta_num_min');

const meta_num_full = new NumberMeta({
  varname: 'numvar',
  label: 'numvar label',
  tags: ['a', 'b', 'c'],
  digits: 2,
  locale: false,
});
writeCase(meta_num_full, 'meta_num_full');

const meta_string = new StringMeta({ varname: 'stringvar' });
writeCase(meta_string, 'meta_string');

const meta_fac = new FactorMeta({
  varname: 'facvar',
  levels: ['l1', 'l2', 'l3'],
});
writeCase(meta_fac, 'meta_fac');

const meta_dt = new DateMeta({ varname: 'datevar' });
writeCase(meta_dt, 'meta_dt');

const meta_dttm = new DatetimeMeta({ varname: 'datetimevar' });
writeCase(meta_dttm, 'meta_dttm');

const meta_href = new HrefMeta({ varname: 'hrefvar' });
writeCase(meta_href, 'meta_href');

const meta_geo = new GeoMeta({
  varname: 'geovar',
  latvar: 'var1',
  longvar: 'var2',
});
writeCase(meta_geo, 'meta_geo');

const meta_grph = new GraphMeta({
  varname: 'graphvar',
  idvarname: 'idvar',
  linkidvarname: 'linkidvar',
});
writeCase(meta_grph, 'meta_grph');

/* ------------------------------------------------------ */
/* inputs                                                 */
/* ------------------------------------------------------ */

const options = ['a', 'b', 'c'];

const input_radio = new RadioInput({ name: 'radio', options });
writeCase(input_radio, 'input_radio');

const input_check = new CheckboxInput({ name: 'check', options });
writeCase(input_check, 'input_check');

const input_select = new SelectInput({ name: 'select', options });
writeCase(input_select, 'input_select');

const input_mselect = new MultiselectInput({ name: 'mselect', options });
writeCase(input_mselect, 'input_mselect');

const input_text = new TextInput({ name: 'text' });
writeCase(input_text, 'input_text');

const input_num = new NumberInput({ name: 'num' });
writeCase(input_num, 'input_num');

/* ------------------------------------------------------ */
/* states                                                 */
/* ------------------------------------------------------ */

const state_layout = new LayoutState({});
writeCase(state_layout, 'state_layout');

const state_label = new LabelState({ varnames: ['a', 'b'] });
writeCase(state_label, 'state_label');

const state_sort = new SortState({ varname: 'a', metatype: 'string' });
writeCase(state_sort, 'state_sort');

const state_catfilt = new CategoryFilterState({
  varname: 'a',
  values: ['a', 'b', 'c'],
  metatype: 'string'
});
writeCase(state_catfilt, 'state_catfilt');

const state_numfilt = new NumberRangeFilterState({ varname: 'a', min: 1 });
writeCase(state_numfilt, 'state_numfilt');

const state_dtfilt = new DateRangeFilterState({
  varname: 'a',
  min: new Date(),
});
writeCase(state_dtfilt, 'state_dtfilt');

const state_dttmfilt = new DatetimeRangeFilterState({
  varname: 'a',
  min: new Date(),
});
writeCase(state_dttmfilt, 'state_dttmfilt');

const displst_min = new DisplayState({
  labels: new LabelState({ varnames: ['a', 'b'] }),
});
writeCase(displst_min, 'displst_min');

const displst = new DisplayState({
  labels: new LabelState({ varnames: ['a', 'b'] }),
  layout: new LayoutState({ ncol: 4 }),
  sort: [
    new SortState({ varname: 'a', metatype: 'string' }),
    new SortState({ varname: 'b', dir: 'desc', metatype: 'string' }),
  ],
  filter: [
    new CategoryFilterState({
      varname: 'a',
      values: ['a', 'b', 'c'],
      metatype: 'string'
    }),
    new NumberRangeFilterState({ varname: 'a', min: 1 }),
  ],
});
writeCase(displst, 'displst');

/* ------------------------------------------------------ */
/* view                                                   */
/* ------------------------------------------------------ */

const view = new View({
  name: 'myview',
  state: displst_min,
});
writeCase(view, 'view');

const view2 = new View({
  name: 'myview2',
  state: displst,
});

/* ------------------------------------------------------ */
/* display                                                */
/* ------------------------------------------------------ */

const selInput = new SelectInput({ name: 'select', options });
const storageInterface = new InputClientSideStorage();
const feedbackInterface = new InputEmailFeedback({
  emailAddress: 'test@test.com',
  includeMetaVars: ['var1'],
})
const inputs = new Inputs({
  storageInterface,
  feedbackInterface,
  inputs: [selInput],
});
const panelsource = new PanelSource('file');

const panelvar = new PanelMeta({
  varname: 'panel',
  paneltype: 'img',
  format: 'svg',
  source: panelsource,
  aspect: 1
});

const displ = new Display({
  name: 'test display',
  keycols: ['a', 'b'],
  keysig: 'asdf',
  metas: [
    new StringMeta({ varname: 'stringvar' }),
    new DateMeta({ varname: 'datevar' }),
  ],
  inputs,
  state: displst,
  views: [view, view2],
  thumbnailurl: 'asdf.svg',
  primaryPanel: 'panel'
});
