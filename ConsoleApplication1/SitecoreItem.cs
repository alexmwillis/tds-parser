using System;
using System.Collections.Generic;

namespace ConsoleApplication1
{
    public class SitecoreItem
    {
        public string Name { get; set; }

        public Guid Id { get; set; }

        public string Path { get; set; }

        public SitecoreItem Parent { get; set; }

        public string TemplateName { get; set; }

        public Guid TemplateId { get; set; }

        public IEnumerable<SitecoreField> Fields { get; set; }

        public IEnumerable<SitecoreItem> Children { get; set; }

        public string RawContent { get; set; }
    }
}