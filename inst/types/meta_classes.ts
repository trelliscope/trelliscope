import {
  IMeta,
  INumberMeta,
  IStringMeta,
  IFactorMeta,
  IDateMeta,
  IDatetimeMeta,
  IHrefMeta,
  IGeoMeta,
  IGraphMeta,
  MetaType,
  GraphDirection,
} from "./meta_types";

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
    if (label === undefined) {
      this.label = varname;
    } else {
      this.label = label;
    }
  }
  validateName(data: any) {
    const row = data[0];
    const hasVar = Object.keys(row).includes(this.varname);
    if (!hasVar) throw new Error(`Specified variable, ${this.varname}, not found in the data.`);
    return hasVar;
  }
  // validateDataValues(data: any) {
  //   return true;
  // }
  // cast(val: any) {
  //   val as 
  // }
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
    if (digits !== undefined) {
      this.digits = digits;
    } else {
      this.digits = null;
    }
    if (locale === undefined) {
      this.locale = true; // default
    } else {
      this.locale = locale;
    }
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
      'geo',
      varname,
      tags,
      label,
      true,
      false
    );
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
  direction: GraphDirection;
  constructor(
    {
      varname,
      label,
      tags,
      idvarname,
      direction
    } : {
      varname: string,
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
    if (direction === undefined) {
      this.direction = 'none'; // default
    } else {
      this.direction = direction;
    }
  };
}
