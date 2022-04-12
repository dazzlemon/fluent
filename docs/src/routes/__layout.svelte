<script lang="ts">
	import Fab, { Icon } from '@smui/fab';
	import { mdiWeatherSunny, mdiWeatherNight } from '@mdi/js'
	import { Svg } from '@smui/common/elements'
	import { onMount } from 'svelte'
	import a11yDark from "svelte-highlight/src/styles/a11y-dark";
	import a11yLight from "svelte-highlight/src/styles/a11y-light";

	let colorTheme: 'dark' | 'light' | undefined = undefined
	$: colors = colorTheme == 'dark' ? a11yDark : a11yLight

	onMount(() => {
		colorTheme = window.matchMedia('(prefers-color-scheme: dark)') ? 'dark' : 'light'
	})
</script>

<svelte:head>
	{@html colors}
	{#if !colorTheme}
		<link rel="stylesheet" href="/smui.css" media="(prefers-color-scheme: light)" />
		<link rel="stylesheet" href="/smui-dark.css" media="screen and (prefers-color-scheme: dark)"/>
	{:else if colorTheme == 'dark'}
		<link rel="stylesheet" href="/smui.css" media="(prefers-color-scheme: light)" />
		<link rel="stylesheet" href="/smui-dark.css" media="screen"/>
	{:else if colorTheme == 'light'}
		<link rel="stylesheet" href="/smui.css"/>
	{/if}
</svelte:head>

<Fab class='darkModeButton' on:click={() => colorTheme = colorTheme == 'dark' ? 'light' : 'dark'}>
	<Icon component={Svg} viewBox="0 0 24 24">
		<path fill='currentColor' d={colorTheme == 'dark' ? mdiWeatherSunny : mdiWeatherNight}/>
	</Icon>
</Fab>
 
<slot />

<style global>
	.darkModeButton {
		width: 48px !important;
		height: 48px !important;
		border-radius: 50%;
		position: fixed !important;
		z-index: 1 !important;
		right: 12px;
		bottom: 12px;
	}
</style>