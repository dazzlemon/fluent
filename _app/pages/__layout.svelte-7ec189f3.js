import{S as T,i as W,s as C,F as E,G as F,H as j,l as y,k as b,w as L,I as D,J as G,d as u,m as g,x as K,K as M,g as _,y as S,L as J,M as O,N as P,q as N,o as w,B as q,v as Q,O as R,P as U,Q as V,R as z,e as k,c as $,b as n,T as A,U as X,a as Y,V as B,W as H}from"../chunks/vendor-978cb2a8.js";function Z(i){let e;return{c(){e=k("link"),this.h()},l(l){e=$(l,"LINK",{rel:!0,href:!0}),this.h()},h(){n(e,"rel","stylesheet"),n(e,"href","/smui.css")},m(l,t){_(l,e,t)},d(l){l&&u(e)}}}function x(i){let e,l,t;return{c(){e=k("link"),l=b(),t=k("link"),this.h()},l(s){e=$(s,"LINK",{rel:!0,href:!0,media:!0}),l=g(s),t=$(s,"LINK",{rel:!0,href:!0,media:!0}),this.h()},h(){n(e,"rel","stylesheet"),n(e,"href","/smui.css"),n(e,"media","(prefers-color-scheme: light)"),n(t,"rel","stylesheet"),n(t,"href","/smui-dark.css"),n(t,"media","screen")},m(s,a){_(s,e,a),_(s,l,a),_(s,t,a)},d(s){s&&u(e),s&&u(l),s&&u(t)}}}function ee(i){let e,l,t;return{c(){e=k("link"),l=b(),t=k("link"),this.h()},l(s){e=$(s,"LINK",{rel:!0,href:!0,media:!0}),l=g(s),t=$(s,"LINK",{rel:!0,href:!0,media:!0}),this.h()},h(){n(e,"rel","stylesheet"),n(e,"href","/smui.css"),n(e,"media","(prefers-color-scheme: light)"),n(t,"rel","stylesheet"),n(t,"href","/smui-dark.css"),n(t,"media","screen and (prefers-color-scheme: dark)")},m(s,a){_(s,e,a),_(s,l,a),_(s,t,a)},d(s){s&&u(e),s&&u(l),s&&u(t)}}}function te(i){let e,l;return{c(){e=A("path"),this.h()},l(t){e=X(t,"path",{fill:!0,d:!0}),Y(e).forEach(u),this.h()},h(){n(e,"fill","currentColor"),n(e,"d",l=i[0]=="dark"?B:H)},m(t,s){_(t,e,s)},p(t,s){s&1&&l!==(l=t[0]=="dark"?B:H)&&n(e,"d",l)},d(t){t&&u(e)}}}function se(i){let e,l;return e=new U({props:{component:V,viewBox:"0 0 24 24",$$slots:{default:[te]},$$scope:{ctx:i}}}),{c(){L(e.$$.fragment)},l(t){K(e.$$.fragment,t)},m(t,s){S(e,t,s),l=!0},p(t,s){const a={};s&17&&(a.$$scope={dirty:s,ctx:t}),e.$set(a)},i(t){l||(N(e.$$.fragment,t),l=!0)},o(t){w(e.$$.fragment,t),l=!1},d(t){q(e,t)}}}function le(i){let e,l,t,s,a,m,h;function p(r,o){if(!r[0])return ee;if(r[0]=="dark")return x;if(r[0]=="light")return Z}let d=p(i),f=d&&d(i);a=new E({props:{class:"darkModeButton",$$slots:{default:[se]},$$scope:{ctx:i}}}),a.$on("click",i[3]);const v=i[2].default,c=F(v,i,i[4],null);return{c(){e=new j,l=y(),f&&f.c(),t=y(),s=b(),L(a.$$.fragment),m=b(),c&&c.c(),this.h()},l(r){const o=D('[data-svelte="svelte-f9ryvf"]',document.head);e=G(o),l=y(),f&&f.l(o),t=y(),o.forEach(u),s=g(r),K(a.$$.fragment,r),m=g(r),c&&c.l(r),this.h()},h(){e.a=l},m(r,o){e.m(i[1],document.head),M(document.head,l),f&&f.m(document.head,null),M(document.head,t),_(r,s,o),S(a,r,o),_(r,m,o),c&&c.m(r,o),h=!0},p(r,[o]){(!h||o&2)&&e.p(r[1]),d!==(d=p(r))&&(f&&f.d(1),f=d&&d(r),f&&(f.c(),f.m(t.parentNode,t)));const I={};o&17&&(I.$$scope={dirty:o,ctx:r}),a.$set(I),c&&c.p&&(!h||o&16)&&J(c,v,r,r[4],h?P(v,r[4],o,null):O(r[4]),null)},i(r){h||(N(a.$$.fragment,r),N(c,r),h=!0)},o(r){w(a.$$.fragment,r),w(c,r),h=!1},d(r){u(l),r&&e.d(),f&&f.d(r),u(t),r&&u(s),q(a,r),r&&u(m),c&&c.d(r)}}}function re(i,e,l){let t,{$$slots:s={},$$scope:a}=e,m;Q(()=>{l(0,m=window.matchMedia("(prefers-color-scheme: dark)")?"dark":"light")});const h=()=>l(0,m=m=="dark"?"light":"dark");return i.$$set=p=>{"$$scope"in p&&l(4,a=p.$$scope)},i.$$.update=()=>{i.$$.dirty&1&&l(1,t=m=="dark"?z:R)},[m,t,s,h,a]}class ie extends T{constructor(e){super();W(this,e,re,le,C,{})}}export{ie as default};