import {
  // meta
  IMeta,
  INumberMeta,
  ICurrencyMeta,
  IStringMeta,
  IFactorMeta,
  IDateMeta,
  IDatetimeMeta,
  IHrefMeta,
  IGeoMeta,
  IGraphMeta,
  MetaType,
  GraphDirection,
  CurrencyCode,
  // inputs
  IInputClientSideStorage,
  IInputEmailFeedback,
  IInputs,
  IInput,
  IRadioInput,
  ICheckboxInput,
  ISelectInput,
  IMultiselectInput,
  ITextInput,
  INumberInput,
  InputType,
  // state
  IDisplayState,
  IState,
  ILayoutState,
  ILabelState,
  ISortState,
  IFilterState,
  ICategoryFilterState,
  INumberRangeFilterState,
  IDateRangeFilterState,
  IDatetimeRangeFilterState,
  StateType,
  LayoutArrangeType,
  SortDirType,
  FilterType,
  // view
  IView,
  // display
  IDisplay,
  PanelFormat,
  PanelType,
  // display list
  IDisplayListItem,
  // config
  AppDataType,
  IConfig,
} from "./types";

/* ------------------------------------------------------ */
/* meta                                                   */
/* ------------------------------------------------------ */

export class Meta implements IMeta {
  type: MetaType;
  varname: string;
  label: string;
  tags: string[];
  filterable: boolean;
  sortable: boolean;
  constructor(
    type: MetaType,
    varname: string,
    tags: string[] = [],
    label: string | undefined = undefined,
    filterable: boolean = true,
    sortable: boolean = true
  ) {
    this.type = type;
    this.varname = varname;
    this.tags = tags;
    this.filterable = filterable;
    this.sortable = sortable;
    this.label = label === undefined ? varname : label;
  }
}

export class NumberMeta extends Meta implements INumberMeta {
  digits: number | null;
  locale: boolean;
  constructor(
    {
      varname,
      label,
      tags,
      digits,
      locale
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[],
      digits?: number,
      locale?: boolean
    }
  ) {
    super('number', varname, tags, label, true, true);
    this.digits = digits === undefined ? null : digits;
    this.locale = locale === undefined ? true : locale;
  };
}

export class CurrencyMeta extends Meta implements ICurrencyMeta {
  code: CurrencyCode;
  constructor(
    {
      varname,
      label,
      tags,
      code
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[],
      code?: CurrencyCode
    }
  ) {
    super('number', varname, tags, label, true, true);
    this.code = code === undefined ? 'USD' : code;
  };
}

export class StringMeta extends Meta implements IStringMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'string',
      varname,
      tags,
      label,
      true,
      true
    );
  };
}

export class FactorMeta extends Meta implements IFactorMeta {
  levels: string[];
  constructor(
    { 
      varname,
      levels,
      label,
      tags
    }: {
      varname: string,
      levels: string[],
      label?: string | undefined,
      tags?: string[],
    }
  ) {
    super(
      'factor',
      varname,
      tags,
      label,
      true,
      true
    );
    this.levels = levels;
  };
}

export class DateMeta extends Meta implements IDateMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'date',
      varname,
      tags,
      label,
      true,
      true
    );
  };
}

export class DatetimeMeta extends Meta implements IDatetimeMeta {
  timezone: string;
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'datetime',
      varname,
      tags,
      label,
      true,
      true
    );
    this.timezone = 'UTC';
  };
}

export class GeoMeta extends Meta implements IGeoMeta {
  latvar: string;
  longvar: string;
  constructor(
    {
      varname,
      latvar,
      longvar,
      label,
      tags
    } : {
      varname: string,
      latvar: string,
      longvar: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'geo',
      varname,
      tags,
      label,
      false, // TODO: change to TRUE when implemented in app
      false
    );
    this.latvar = latvar;
    this.longvar = longvar;
  };
}

export class HrefMeta extends Meta implements IHrefMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'href',
      varname,
      tags,
      label,
      false,
      false
    );
  };
}

export class GraphMeta extends Meta implements IGraphMeta {
  idvarname: string;
  linkidvarname: string;
  labelvarname: string;
  params: object;
  direction: GraphDirection;
  constructor(
    {
      varname,
      label,
      tags,
      idvarname,
      linkidvarname,
      labelvarname,
      // params,
      direction
    } : {
      varname: string,
      linkidvarname: string,
      labelvarname?: string,
      params?: object,
      label?: string | undefined,
      tags?: string[],
      idvarname: string,
      direction?: GraphDirection
    }
  ) {
    super(
      'graph',
      varname,
      tags,
      label,
      true,
      false
    );
    this.idvarname = idvarname;
    this.linkidvarname = linkidvarname;
    this.labelvarname = labelvarname === undefined ? idvarname : labelvarname;
    this.params = {};
    this.direction = direction === undefined ? 'none' : direction;
  };
}

/* ------------------------------------------------------ */
/* inputs                                                 */
/* ------------------------------------------------------ */


export class InputClientSideStorage implements IInputClientSideStorage {
  type: "localStorage";
  constructor() {
    this.type = "localStorage";
  }
}

export class InputEmailFeedback implements IInputEmailFeedback {
  emailAddress: string;
  includeMetaVars: string[];
  constructor(
    {
      emailAddress,
      includeMetaVars,
    } : {
      emailAddress: string,
      includeMetaVars: string[],
    }
  ) {
    this.emailAddress = emailAddress;
    this.includeMetaVars = includeMetaVars;
  }
}

export class Inputs implements IInputs {
  inputs: IInput[];
  storageInterface: IInputClientSideStorage;
  feedbackInterface: IInputEmailFeedback;
  constructor(
    {
      inputs,
      storageInterface,
      feedbackInterface,
    } : {
      inputs: IInput[],
      storageInterface: IInputClientSideStorage,
      feedbackInterface: IInputEmailFeedback,
    }
  ) {
    this.inputs = inputs;
    this.storageInterface = storageInterface;
    this.feedbackInterface = feedbackInterface;
  }
}

export class Input implements IInput {
  type: InputType;
  name: string;
  label: string;
  active: boolean;
  constructor(
    type: InputType,
    name: string,
    label: string | undefined = undefined,
    active: boolean | undefined = true,
  ) {
    this.type = type;
    this.name = name;
    this.active = active === undefined ? true: active;
    this.label = label === undefined ? name: label;
  }
}

export class RadioInput extends Input implements IRadioInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('radio', name, label, active);
    this.options = options;
  };
}

export class CheckboxInput extends Input implements ICheckboxInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('checkbox', name, label, active);
    this.options = options;
  };
}

export class SelectInput extends Input implements ISelectInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('select', name, label, active);
    this.options = options;
  };
}

export class MultiselectInput extends Input implements IMultiselectInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('multiselect', name, label, active);
    this.options = options;
  };
}

export class TextInput extends Input implements ITextInput {
  width: number;
  height: number
  constructor(
    {
      name,
      label,
      active,
      width,
      height,
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      width?: number | undefined,
      height?: number | undefined,
    }
  ) {
    super('text', name, label, active);
    this.width = width === undefined ? 80 : width;
    this.height = height === undefined ? 3 : height;
  };
}

export class NumberInput extends Input implements INumberInput {
  constructor(
    {
      name,
      label,
      active,
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
    }
  ) {
    super('number', name, label, active);
  };
}

/* ------------------------------------------------------ */
/* states                                                 */
/* ------------------------------------------------------ */

export class DisplayState implements IDisplayState {
  layout: ILayoutState;
  labels: ILabelState;
  sort: ISortState[];
  filter: IFilterState[];
  constructor(
    {
      layout,
      labels,
      sort,
      filter,
    } : {
      layout?: ILayoutState | undefined,
      labels: ILabelState,
      sort?: ISortState[] | undefined,
      filter?: IFilterState[] | undefined,
    }
  ) {
    this.layout = layout === undefined ? new LayoutState({}) : layout;
    this.labels = labels;
    this.sort = sort === undefined ? [] : sort;
    this.filter = filter === undefined ? [] : filter;
  }
}

export class State implements IState {
  type: StateType;
  constructor(
    type: StateType,
  ) {
    this.type = type;
  }
}

export class LayoutState extends State implements ILayoutState {
  nrow: number;
  ncol: number;
  arrange: LayoutArrangeType;
  page: number;
  constructor(
    {
      nrow,
      ncol,
      arrange,
      page,
    } : {
      nrow?: number | undefined,
      ncol?: number | undefined,
      arrange?: LayoutArrangeType | undefined,
      page?: number | undefined,
    }
  ) {
    super('layout');
    this.nrow = nrow === undefined ? 1 : nrow;
    this.ncol = ncol === undefined ? 1 : ncol;
    this.page = page === undefined ? 1 : page;
    this.arrange = arrange === undefined ? 'rows' : arrange;
  };
}

export class LabelState extends State implements ILabelState {
  varnames: string[];
  constructor(
    {
      varnames,
    } : {
      varnames: string[],
    }
  ) {
    super('labels');
    this.varnames = varnames;
  };
}

export class SortState extends State implements ISortState {
  varname: string;
  dir: SortDirType;
  constructor(
    {
      varname,
      dir,
    } : {
      varname: string,
      dir?: SortDirType | undefined,
    }
  ) {
    super('sort');
    this.varname = varname;
    this.dir = dir === undefined ? 'asc' : dir;
  };
}

export class FilterState extends State implements IFilterState {
  varname: string;
  filtertype: FilterType;
  constructor(
    {
      varname,
      filtertype,
    } : {
      varname: string,
      filtertype: FilterType,
    }
  ) {
    super('filter');
    this.varname = varname;
    this.filtertype = filtertype;
  };
}

export class CategoryFilterState extends FilterState implements ICategoryFilterState {
  regexp: string | null;
  values: string[];
  constructor(
    {
      varname,
      regexp,
      values,
    } : {
      varname: string,
      regexp?: string | undefined,
      values?: string[] | undefined,
    }
  ) {
    super({ varname, filtertype: 'category' });
    this.regexp = regexp === undefined ? null : regexp;
    this.values = values === undefined ? [] : values;
  };
}

export class NumberRangeFilterState extends FilterState implements INumberRangeFilterState {
  min: number | null;
  max: number | null;
  constructor(
    {
      varname,
      min,
      max,
    } : {
      varname: string,
      min?: number | undefined,
      max?: number | undefined,
    }
  ) {
    super({ varname, filtertype: 'numberrange' });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

export class DateRangeFilterState extends FilterState implements IDateRangeFilterState {
  min: Date | null;
  max: Date | null;
  constructor(
    {
      varname,
      min,
      max,
    } : {
      varname: string,
      min?: Date | undefined,
      max?: Date | undefined,
    }
  ) {
    super({ varname, filtertype: 'daterange' });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

export class DatetimeRangeFilterState extends FilterState implements IDatetimeRangeFilterState {
  min: Date | null;
  max: Date | null;
  constructor(
    {
      varname,
      min,
      max,
    } : {
      varname: string,
      min?: Date | undefined,
      max?: Date | undefined,
    }
  ) {
    super({ varname, filtertype: 'datetimerange' });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

/* ------------------------------------------------------ */
/* view                                                   */
/* ------------------------------------------------------ */

export class View implements IView {
  name: string;
  state: IDisplayState;
  constructor(
    {
      name,
      state,
    } : {
      name: string,
      state: IDisplayState,
    }
  ) {
    this.name = name;
    this.state = state;
  }
}

/* ------------------------------------------------------ */
/* display                                                */
/* ------------------------------------------------------ */

export class Display implements IDisplay {
  name: string;
  description: string;
  tags: string[];
  keycols: string[];
  keysig: string;
  metas: IMeta[];
  inputs: IInputs | null;
  state: IDisplayState;
  views: IView[];
  paneltype: PanelType;
  panelformat?: PanelFormat;
  thumbnailurl: string;
  panelaspect: number;
  constructor(
    {
      name,
      description,
      tags,
      keycols,
      keysig,
      metas,
      inputs,
      state,
      views,
      paneltype,
      panelformat,
      thumbnailurl,
      panelaspect,
    } : {
      name: string,
      description?: string,
      tags?: string[],
      keycols: string[],
      keysig: string,
      metas: IMeta[],
      inputs?: IInputs | null,
      state: IDisplayState,
      views?: IView[] | undefined,
      paneltype: PanelType,
      panelformat: PanelFormat | undefined,
      thumbnailurl: string,
      panelaspect: number,
    }
  ) {
    this.name = name;
    this.description = description === undefined ? name : description;
    this.tags = tags === undefined ? [] : tags;
    this.keycols = keycols;
    this.keysig = keysig;
    this.metas = metas;
    this.inputs = inputs === undefined ? null : inputs;
    this.state = state;
    this.views = views === undefined ? [] : views;
    this.paneltype = paneltype;
    if (panelformat !== undefined) {
      this.panelformat = panelformat;
    }
    this.panelaspect = panelaspect;
    this.thumbnailurl = thumbnailurl;
  }
}

/* ------------------------------------------------------ */
/* display list                                           */
/* ------------------------------------------------------ */

export class DisplayListItem implements IDisplayListItem {
  name: string;
  description: string;
  tags: string[];
  keysig: string;
  thumbnailurl: string;
  constructor(
    {
      name,
      description,
      tags,
      keysig,
      thumbnailurl,
    } : {
      name: string,
      description?: string,
      tags?: string[],
      keysig: string,
      thumbnailurl: string,
    }
  ) {
    this.name = name;
    this.description = description === undefined ? name : description;
    this.tags = tags === undefined ? [] : tags;
    this.keysig = keysig;
    this.thumbnailurl = thumbnailurl;
  }
}

// var displayList: DisplayListItem[] = [...]

/* ------------------------------------------------------ */
/* config                                                 */
/* ------------------------------------------------------ */

export class Config implements IConfig {
  name: string;
  datatype: AppDataType;
  id: string;
  constructor(
    {
      name,
      datatype,
      id,
    } : {
      name: string;
      datatype: AppDataType;
      id: string;
    }
  ) {
    this.name = name;
    this.datatype = datatype;
    this.id = id;
  }
}
